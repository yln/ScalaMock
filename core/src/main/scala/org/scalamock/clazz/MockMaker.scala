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
    
    class Method(val m: MethodSymbol) {
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
      val paramCount = info.paramLists.map(_.length).sum
      val fakeType = s"org.scalamock.function.${if (stub) "Stub" else "Mock"}Function${paramCount}"
      val fake = fakeType + (paramTypes :+ info.finalResultType).map(p => toMockType(p, false)).mkString("[", ", ", "]")
      val overloads = methods.filter { m => m.name == name }
      val overloadIndex = overloads.indexOf(m)
      val overloadDisambiguation =
        if (overloadIndex > 0) 
          (1 to overloadIndex).map(i => s"x$i: scala.Predef.DummyImplicit").mkString("(implicit ", ", ", ")")
        else
          ""
      val fakeName = s"fake$$$name$$$overloadIndex"
      val matcherFunction = s"org.scalamock.function.FunctionAdapter${paramCount}"
      val matcherParamTypes = info.paramLists.flatten.map { p => toMockType(p.info, false) }
      val matcherType = s"$matcherFunction${(matcherParamTypes :+ "Boolean").mkString("[", ", ", "]")}"
      
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
            case TypeRef(pre, sym, args) if pre == typeToMock => internal.typeRef(NoPrefix, sym, args)
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
          (!m.isAccessor || isDeferred(m)) && !isBridge(m) &&
          !m.isParamWithDefault // see issue #43
      }.map(new Method(_))
    val stableMethods = methodsToMock.filter(!_.isStable)

    val forwarders = methodsToMock map { m =>
        if (m.isStable)
          ctx.parse(s"val ${m.name} = null.asInstanceOf[${m.res}]")
        else
          ctx.parse(s"override def ${m.name}${m.tparams}${m.paramss} = ${m.fakeName}${m.flatParams}.asInstanceOf[${m.res}]")
      }

    val typeName = s"${typeToMock.typeSymbol.name}${if (typeToMock.typeArgs.isEmpty) "" else typeToMock.typeArgs.mkString("[", ", ", "]")}"
    val mocks = stableMethods.map { m =>
        val name = s"$typeName.${m.name}${m.tparams}"
        ctx.parse(s"""val ${m.fakeName} = new ${m.fake}(mock$$special$$context, Symbol("<" + mock$$special$$mockName + "> $name"))""")
      }
    
    def constraintSetters(constraint: String) = stableMethods.flatMap { m =>
        List(
          ctx.parse(s"def ${m.name}${m.tparams}${m.mockParamss}${m.overloadDisambiguation} = ${m.fakeName}.$constraint${m.flatParams}"),
          ctx.parse(s"def ${m.name}${m.tparams}(matcher: ${m.matcherType})${m.overloadDisambiguation} = ${m.fakeName}.$constraint(matcher)")
        )
      }
    
    val constraints =
      if (stub)
        q"""val when = new { ..${constraintSetters("when")} }
            val verify = new { ..${constraintSetters("verify")} }"""
      else
        q"""val expects = new { ..${constraintSetters("expects")} }
            val stubs = new { ..${constraintSetters("stubs")} }"""
            
    val mockName = optionalName match {
      case (Some(name)) => q"val mock$$special$$mockName = $name"
      case None => q"val mock$$special$$mockName = mock$$special$$context.generateMockDefaultName(${if (stub) "stub" else "mock"}).name"
    }

    def make() = {
      val mock = q"""
          class Mock(mock$$special$$context: org.scalamock.context.MockContext) extends $typeToMock {
            $mockName
            ..$forwarders
            ..$mocks
            ..$constraints
          }
  
          new Mock($mockContext)
        """

//      println(show(mock))
      ctx.Expr(mock)
    }
  }
}
