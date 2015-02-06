// Copyright (c) 2011-2015 ScalaMock Contributors (https://github.com/paulbutcher/ScalaMock/graphs/contributors)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

package org.scalamock.clazz

import org.scalamock.context.MockContext
import org.scalamock.function._

import scala.reflect.macros.whitebox.Context

//! TODO - get rid of this nasty two-stage construction when https://issues.scala-lang.org/browse/SI-5712 is fixed
class MockMaker[C <: Context](val ctx: C) {
  class MockMakerInner[T: ctx.WeakTypeTag](mockContext: ctx.Expr[MockContext], stub: Boolean, mockName: Option[ctx.Expr[String]]) {
    import ctx.universe._
    
    class Method(val m: Symbol, val index: Int) {
      val info = m.infoIn(typeToImplement)
      val name = m.name.toString
      val tparams = info.typeParams match {
          case Nil => ""
          case tps => tps.map(_.name).mkString("[", ", ", "]")
        }
      val res = info.finalResultType
      val paramss = info.paramLists.map { ps =>
          ps.map(p => s"${p.name}: ${p.infoIn(typeToImplement)}").mkString("(", ", ", ")")
        }.mkString("")
      val mockParamss = info.paramLists.map { ps =>
          ps.map(p => s"${p.name}: org.scalamock.matchers.MockParameter[${p.infoIn(typeToImplement).erasure}]").mkString("(", ", ", ")")
        }.mkString("") 
      val paramTypes = info.paramLists.flatten.map { p => p.infoIn(typeToImplement).erasure }
      val flatParams = info.paramLists.flatten.map { p => p.name }.mkString("(", ", ", ")")
      val mockTypes = (paramTypes :+ res).mkString("[", ", ", "]") 
      val mockName = "fake$" + index
      val paramCount = info.paramLists.map(_.length).sum
    }

    def isMemberOfObject(m: Symbol) = TypeTag.Object.tpe.member(m.name) != NoSymbol

    val typeToImplement = weakTypeOf[T]
    val methodsToImplement = typeToImplement.members.filter { m =>
        m.isMethod && !isMemberOfObject(m)
      }.zipWithIndex.map { case (m, i) => new Method(m, i) }

    val methods = methodsToImplement map { m =>
        ctx.parse(s"def ${m.name}${m.tparams}${m.paramss} = ${m.mockName}${m.flatParams}")
      }
    
    val mocks = methodsToImplement.map { m =>
        ctx.parse(s"val ${m.mockName} = new org.scalamock.function.MockFunction${m.paramCount}${m.mockTypes}(mockContext, 'dummyName)")
      }
    
    val expecters = methodsToImplement.map { m =>
        ctx.parse(s"def ${m.name}${m.tparams}${m.mockParamss} = ${m.mockName}.expects${m.flatParams}")
      }

    def make() = {
      val mock = q"""
          class MockThing(mockContext: org.scalamock.context.MockContext) extends $typeToImplement {
            ..$methods
            ..$mocks
            val expects = new {
              ..$expecters
            }
          }
  
          new MockThing($mockContext)
        """

//      println(show(mock))
      ctx.Expr(mock)
    }
  }
}
