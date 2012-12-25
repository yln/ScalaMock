// Copyright (c) 2011-2012 Paul Butcher
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

package org.scalamock.typemacro

import org.scalamock._

trait Mock {
  import language.experimental.macros

  def mock[T] = macro MockImpl.mock[T]
}

object Mock {
  import language.experimental.macros

  type MockClass[T] = macro MockImpl.MockClass[T]
}

object MockImpl {
  import reflect.macros.Context

  def mock[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val t = weakTypeOf[T]
    c.Expr(q"new org.scalamock.typemacro.Mock.MockClass[${t.typeSymbol.name}]")
  }

  def MockClass[T: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    import Flag._
    import definitions._

    val typeToMock = weakTypeOf[T]

    // Convert a methodType into its ultimate result type
    // For nullary and normal methods, this is just the result type
    // For curried methods, this is the final result type of the result type
    def finalResultType(methodType: Type): Type = methodType match {
      case NullaryMethodType(result) => result 
      case MethodType(_, result) => finalResultType(result)
      case PolyType(_, result) => finalResultType(result)
      case _ => methodType
    }

    def mockFunctionClass(paramCount: Int) = {
      val name = TypeName(s"MockFunction$paramCount")
      tq"org.scalamock.$name"
    }    

    def isPathDependentThis(t: Type): Boolean = t match {
      case TypeRef(pre, _, _) => isPathDependentThis(pre)
      case ThisType(tpe) => tpe == typeToMock.typeSymbol
      case _ => false
    }
      
    def paramType(t: Type) = t match {
      case TypeRef(pre, sym, args) if sym == JavaRepeatedParamClass =>
        TypeTree(TypeRef(pre, RepeatedParamClass, args))
      case TypeRef(pre, sym, args) if isPathDependentThis(t) =>
        AppliedTypeTree(Ident(TypeName(sym.name.toString)), args map { a => TypeTree(a) })
      case _ =>
        TypeTree(t)
    }
    
    // Convert a methodType into a list of lists of params:
    // UnaryMethodType => Nil
    // Normal method => List(List(p1, p2, ...))
    // Curried method => List(List(p1, p2, ...), List(q1, q2, ...), ...)
    def paramss(methodType: Type): List[List[Symbol]] = methodType match {
      case MethodType(params, result) => params :: paramss(result)
      case PolyType(_, result) => paramss(result)
      case _ => Nil
    }

    def paramCount(methodType: Type): Int = methodType match {
      case MethodType(params, result) => params.length + paramCount(result)
      case PolyType(_, result) => paramCount(result)
      case _ => 0
    }
    
    def paramTypes(methodType: Type): List[Type] =
      paramss(methodType).flatten map { _.typeSignature }

    def mockFunctionName(i: Int) = TermName(s"mock_$i")
      
    def forwarderImpl(m: MethodSymbol, i: Int) = {
      val mt = m.typeSignature
      if (m.isStable) {
        ValDef(
          Modifiers(), 
          TermName(m.name.toString), 
          TypeTree(mt), 
          TypeApply(
            Select(
              Literal(Constant(null)), 
              TermName("asInstanceOf")),
            List(TypeTree(mt))))
      } else {
        val pss = paramss(mt)
        val ps = pss.flatten map { p => Ident(TermName(p.name.toString)) }
        val body = q"${mockFunctionName(i)}(..$ps)"
        DefDef(
          Modifiers(OVERRIDE),
          m.name, 
          m.typeParams map { p => TypeDef(p) }, 
          pss map { ps =>
              ps map { p =>
                ValDef(
                  Modifiers(PARAM | (if (p.isImplicit) IMPLICIT else NoFlags)),
                  TermName(p.name.toString),
                  paramType(p.typeSignature),
                  EmptyTree)
              }
            },
          paramType(finalResultType(mt)),
          body)
      }
    }

    def mockMethod(m: MethodSymbol, i: Int) = {
      val mt = m.typeSignature
      val clazz = mockFunctionClass(paramCount(mt))
      val types = (paramTypes(mt) map { p => paramType(p) }) :+ paramType(finalResultType(mt))
      q"val ${mockFunctionName(i)} = new $clazz[..$types](factory, Symbol(${m.name.toString}))"
    }

    // Add DummyImplicit sentinel parameters to overloaded methods to avoid problems with
    // ambiguity in the face of type erasure. See:
    // http://groups.google.com/group/scala-user/browse_thread/thread/95acee0572cfa407/95d41ac32d36f743#95d41ac32d36f743
    def overloadDisambiguation(m: MethodSymbol): List[List[ValDef]] = {
      val index = m.owner.typeSignature.member(m.name).asTerm.alternatives.indexOf(m)
      assert(index >= 0)
      if (index > 0)
        List(
          List.range(1, index) map { i =>
            ValDef(
              Modifiers(PARAM | IMPLICIT),
              TermName(s"sentinel$i"),
              TypeTree(typeOf[DummyImplicit]),
              EmptyTree)
          })
      else
        Nil
    }

    def expectationForwarder(m: MethodSymbol, i: Int) = {
      val mt = m.typeSignature
      val pss = paramss(mt)
      val ps = pss.flatten map { p => Ident(TermName(p.name.toString)) }
      val body = q"${mockFunctionName(i)}.expects(..$ps)"
      val args = (pss map { ps =>
          ps map { p =>
            ValDef(
              Modifiers(PARAM | (if (p.isImplicit) IMPLICIT else NoFlags)),
              TermName(p.name.toString),
              AppliedTypeTree(
                Select(Select(Ident(TermName("org")), TermName("scalamock")), TypeName("MockParameter")),
                List(paramType(p.typeSignature))),
              EmptyTree)
          }
        }) ++ overloadDisambiguation(m)
      DefDef(
        Modifiers(),
        m.name, 
        m.typeParams map { p => TypeDef(p) }, 
        args,
        AppliedTypeTree(
          Select(Select(Ident(TermName("org")), TermName("scalamock")), TypeName("CallHandler")),
          List(paramType(finalResultType(mt)))),
        body)
    }

    def getPackage(sym: Symbol): RefTree = 
      if (sym.owner == c.mirror.RootClass)
        Ident(sym.name.toTermName)
      else
        Select(getPackage(sym.owner), sym.name.toTermName)

    def isMemberOfObject(m: Symbol) = TypeTag.Object.tpe.member(m.name) != NoSymbol

    val mockName = c.freshName(typeToMock.typeSymbol.name).toTypeName
    val mockPackage = getPackage(typeToMock.typeSymbol.owner)
    val methodsNotInObject =
      typeToMock.members filter (m => m.isMethod && !isMemberOfObject(m)) map (_.asMethod)
    val methodsToMock = methodsNotInObject.filter { m =>
        !m.isConstructor && (!(m.isStable || m.isAccessor) ||
          m.asInstanceOf[reflect.internal.HasFlags].isDeferred) //! TODO - stop using internal if/when this gets into the API
      }.zipWithIndex.toList
    val forwarders = methodsToMock map { case (m, i) => forwarderImpl(m, i) }
    val mocks = methodsToMock map { case (m, i) => mockMethod(m, i) }
    val expectationForwarders = methodsToMock map { case (m, i) => expectationForwarder(m, i) }

    val classDef = q"""
      class $mockName(implicit factory: org.scalamock.MockFactoryBase) extends ${typeToMock.typeSymbol.name} {
        ..$forwarders
        ..$mocks
        class Expectations {
          ..$expectationForwarders
        }
        val expects = new Expectations
      }"""

    // println("================")
    // println(classDef)
    // println("================")
    // println(showRaw(classDef))
    // println("================")

    c.introduceTopLevel(mockPackage.toString, classDef)
    Select(mockPackage, mockName)
  }
}