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
  class MockMakerInner[T: ctx.WeakTypeTag](mockContext: ctx.Expr[MockContext], stub: Boolean, optionalName: Option[ctx.Expr[String]]) {
    import ctx.universe._
    import definitions._
    
    def mockFn(arity: Int) = arity match {
      case 0 => typeOf[org.scalamock.function.MockFunction0[_]]
      case 1 => typeOf[org.scalamock.function.MockFunction1[_, _]]
      case 2 => typeOf[org.scalamock.function.MockFunction2[_, _, _]]
      case 3 => typeOf[org.scalamock.function.MockFunction3[_, _, _, _]]
      case 4 => typeOf[org.scalamock.function.MockFunction4[_, _, _, _, _]]
      case 5 => typeOf[org.scalamock.function.MockFunction5[_, _, _, _, _, _]]
      case 6 => typeOf[org.scalamock.function.MockFunction6[_, _, _, _, _, _, _]]
      case 7 => typeOf[org.scalamock.function.MockFunction7[_, _, _, _, _, _, _, _]]
      case 8 => typeOf[org.scalamock.function.MockFunction8[_, _, _, _, _, _, _, _, _]]
      case 9 => typeOf[org.scalamock.function.MockFunction9[_, _, _, _, _, _, _, _, _, _]]
    }
    
    class Method(val m: MethodSymbol) {
      val name = m.name.toTermName
      val tparams = m.typeParams.map(ctx.internal.typeDef(_))
      val paramss = m.paramLists.map(_.map(ctx.internal.valDef(_)))
      val params = m.paramLists.flatten.map(_.name)
      val paramTypes = m.paramLists.flatten.map(_.info)
      val resultType = m.returnType
      val fakeName = ctx.freshName(name)
      val paramCount = m.paramLists.map(_.length).sum
      val fakeTypeParams = paramTypes :+ resultType
      val fakeFn = mockFn(paramCount).typeConstructor.typeSymbol
      val constraintParams = m.paramLists.map { ps =>
          ps.map { p => 
            val paramType = p.info match {
              case TypeRef(_, sym, args) if sym == RepeatedParamClass || sym == JavaRepeatedParamClass => tq"Seq[${args.head}]"
              case t => tq"$t"
            }
            q"val ${p.name.toTermName}: org.scalamock.matchers.MockParameter[$paramType]"
          }
        }
      
      def forwarder = {
        if (m.isStable)
          q"val $name = null.asInstanceOf[$resultType]"
        else
          q"override def $name[..$tparams](...$paramss): $resultType = $fakeName(..$params).asInstanceOf[$resultType]"
      }
      def fake = q"val $fakeName = new ${fakeFn}[..$fakeTypeParams]($mockContext, 'dummyName)"
      def expects = q"def $name[..$tparams](...$constraintParams) = $fakeName.expects(..$params)"
    }
    
    val typeToMock = weakTypeOf[T]
    val typeArgs = typeToMock.typeArgs.map(_.typeSymbol)
    val typeParams = typeToMock.typeConstructor.typeSymbol.asType.typeParams
    
    def isMemberOfObject(m: Symbol) = typeOf[Object].member(m.name) != NoSymbol
    def isDeferred(m: MethodSymbol) = m.asInstanceOf[reflect.internal.HasFlags].isDeferred
    
    val methods = typeToMock.members.collect { 
      case m if m.isMethod => m.asMethod
    }.collect {
      case m if !isMemberOfObject(m) && !m.isConstructor && 
                (!m.isAccessor || isDeferred(m)) => new Method(m)
    }
    val unstableMethods = methods.filter(!_.m.isStable)
    
    val forwarders = methods.map(_.forwarder)
    val fakes = unstableMethods.map(_.fake)
    val expecters = unstableMethods.map(_.expects)
    
    val mock = TypeName(ctx.freshName(typeToMock.typeSymbol.name.toString))

    def make() = {
      val t = q"""
          class $mock extends $typeToMock {
            ..$forwarders
            ..$fakes
            val expects = new {
              ..$expecters
            }
          }
  
          new $mock
        """
      ctx.internal.substituteSymbols(t, typeParams, typeArgs)
    }
  }
}
