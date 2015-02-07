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
    import definitions._
    
    class Method(val m: MethodSymbol, val index: Int) {
      val info = m.infoIn(typeToMock)
      val isStable = m.isStable
      val name = m.name
      val typeParams = info.typeParams.map(_.name.toString)
      val tparams = if (typeParams.isEmpty) "" else typeParams.mkString("[", ", ", "]") 
      val res = fixTypePaths(info.finalResultType)
      val paramss = info.paramLists.map { ps =>
          ps.map {p => 
            s"${if(p.isImplicit) "implicit" else ""} ${p.name}: ${p.info}"
          }.mkString("(", ", ", ")")
        }.mkString("")
      val mockParamss = info.paramLists.map { ps =>
          ps.map(p => s"${p.name}: ${toMockType(p.info, true)}").mkString("(", ", ", ")")
        }.mkString("") 
      val flatParams = info.paramLists.flatten.map { p => p.name }.mkString("(", ", ", ")")
      val paramTypes = info.paramLists.flatten.map { p => p.info }
      val mockName = "fake$" + index
      val paramCount = info.paramLists.map(_.length).sum
      val fakeType = s"org.scalamock.function.MockFunction${paramCount}"
      val fake = fakeType + (paramTypes :+ info.finalResultType).map(p => toMockType(p, false)).mkString("[", ", ", "]")
      val overloads = methods.filter { m => m.name == name }
      val overloadIndex = overloads.indexOf(m)
      val overloadDisambiguation =
        if (overloadIndex > 0) 
          (1 to overloadIndex).map(i => s"x$i: scala.Predef.DummyImplicit").mkString("(implicit ", ", ", ")")
        else
          ""
      
      def toMockType(paramType: Type, param: Boolean) = {
        if (!param && paramType.exists(x => typeParams.contains(x.toString))) {
          "Any"
        } else {
          val t = fixTypePaths(paramType) match {
            case TypeRef(_, sym, args) if sym == RepeatedParamClass || sym == JavaRepeatedParamClass => s"Seq[${args.head}]"
            case TypeRef(_, sym, args) if sym == ByNameParamClass => args.head.toString
            case t => t.toString
          }
          if (param) s"org.scalamock.matchers.MockParameter[$t]" else t
        }
      }
      
      def fixTypePaths(paramType: Type) = {
        paramType.map { x =>
          x match {
            case TypeRef(pre, sym, _) if pre == typeToMock => internal.typeRef(NoPrefix, sym, List())
            case _ => x
          }
        }
      }
    }

    def isMemberOfObject(m: Symbol) = TypeTag.Object.tpe.member(m.name) != NoSymbol
    def isBridge(m: MethodSymbol) = m.asInstanceOf[reflect.internal.HasFlags].hasFlag(reflect.internal.Flags.BRIDGE)
    def isDeferred(m: MethodSymbol) = m.asInstanceOf[reflect.internal.HasFlags].isDeferred

    val typeToMock = weakTypeOf[T]
    val methods = typeToMock.members.toIndexedSeq.filter(_.isMethod).map(_.asMethod)
    val methodsToMock = methods.filter { m =>
        !m.isConstructor && !isMemberOfObject(m) && !m.isPrivate &&
          m.privateWithin == NoSymbol && !m.isFinal &&
          (!m.isAccessor || isDeferred(m)) && !isBridge(m)
      }.zipWithIndex.map { case (m, i) => new Method(m, i) }
    val stableMethods = methodsToMock.filter(!_.isStable)

    val forwarders = methodsToMock map { m =>
        if (m.isStable)
          ctx.parse(s"val ${m.name} = null.asInstanceOf[${m.res}]")
        else
          ctx.parse(s"override def ${m.name}${m.tparams}${m.paramss} = ${m.mockName}${m.flatParams}.asInstanceOf[${m.res}]")
      }
    
    val mocks = stableMethods.map { m =>
        ctx.parse(s"val ${m.mockName} = new ${m.fake}(mockContext, 'dummyName)")
      }
    
    val expecters = stableMethods.map { m =>
        ctx.parse(s"def ${m.name}${m.tparams}${m.mockParamss}${m.overloadDisambiguation} = ${m.mockName}.expects${m.flatParams}")
      }

    def make() = {
      val mock = q"""
          class Mock(mockContext: org.scalamock.context.MockContext) extends $typeToMock {
            ..$forwarders
            ..$mocks
            val expects = new {
              ..$expecters
            }
          }
  
          new Mock($mockContext)
        """

//      println(show(mock))
      ctx.Expr(mock)
    }
  }
}
