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

  type mock[T](implicit factory: MockFactoryBase) = macro MockImpl.mock[T]
}

object MockImpl {
  import reflect.macros.Context

  def mock[T: c.WeakTypeTag](c: Context)(factory: c.Expr[MockFactoryBase]): c.Tree = {
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

    def mockFunctionClass(paramCount: Int): Type = paramCount match {
      case 0 => typeOf[MockFunction0[_]]
      case 1 => typeOf[MockFunction1[_, _]]
      case 2 => typeOf[MockFunction2[_, _, _]]
      case 3 => typeOf[MockFunction3[_, _, _, _]]
      case 4 => typeOf[MockFunction4[_, _, _, _, _]]
      case 5 => typeOf[MockFunction5[_, _, _, _, _, _]]
      case 6 => typeOf[MockFunction6[_, _, _, _, _, _, _]]
      case 7 => typeOf[MockFunction7[_, _, _, _, _, _, _, _]]
      case 8 => typeOf[MockFunction8[_, _, _, _, _, _, _, _, _]]
      case 9 => typeOf[MockFunction9[_, _, _, _, _, _, _, _, _, _]]
      case _ => c.abort(c.enclosingPosition, "ScalaMock: Can't handle methods with more than 9 parameters (yet)")
    }
    
    def stubFunctionClass(paramCount: Int): Type = paramCount match {
      case 0 => typeOf[StubFunction0[_]]
      case 1 => typeOf[StubFunction1[_, _]]
      case 2 => typeOf[StubFunction2[_, _, _]]
      case 3 => typeOf[StubFunction3[_, _, _, _]]
      case 4 => typeOf[StubFunction4[_, _, _, _, _]]
      case 5 => typeOf[StubFunction5[_, _, _, _, _, _]]
      case 6 => typeOf[StubFunction6[_, _, _, _, _, _, _]]
      case 7 => typeOf[StubFunction7[_, _, _, _, _, _, _, _]]
      case 8 => typeOf[StubFunction8[_, _, _, _, _, _, _, _, _]]
      case 9 => typeOf[StubFunction9[_, _, _, _, _, _, _, _, _, _]]
      case _ => c.abort(c.enclosingPosition, "ScalaMock: Can't handle methods with more than 9 parameters (yet)")
    }
    
    def classType(paramCount: Int) = mockFunctionClass(paramCount)
      
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

    def buildParams(methodType: Type) =
      paramss(methodType) map { params =>
        params map { p =>
          ValDef(
            Modifiers(PARAM | (if (p.isImplicit) IMPLICIT else NoFlags)),
            TermName(p.name.toString),
            paramType(p.typeSignature),
            EmptyTree)
        }
      }
    
    // def <|name|>(p1: T1, p2: T2, ...): T = <|mockname|>(p1, p2, ...)
    def methodDef(m: MethodSymbol, methodType: Type, body: Tree): DefDef = {
      val params = buildParams(methodType)
      DefDef(
        Modifiers(OVERRIDE),
        m.name, 
        m.typeParams map { p => TypeDef(p) }, 
        params,
        paramType(finalResultType(methodType)),
        body)
    }
      
    def forwarderImpl(m: MethodSymbol) = {
      val mt = resolvedType(m)
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
        val body = q"???"
        methodDef(m, mt, body)
      }
    }

    def mockMethod(m: MethodSymbol) = {
      val mt = resolvedType(m)
      val clazz = classType(paramCount(mt))
      val types = (paramTypes(mt) map { p => paramType(p) }) :+ paramType(finalResultType(mt))
      Apply(
        Select(
          New(
            AppliedTypeTree(
              Ident(clazz.typeSymbol),
              types)),
          TermName("<init>")),
        List(
          factory.tree, 
          Apply(
            Select(Select(Ident(TermName("scala")), TermName("Symbol")), TermName("apply")),
            List(Literal(Constant(m.name.toString))))))
    }

    def getPackage(sym: Symbol): RefTree = 
      if (sym.owner == c.mirror.RootClass)
        Ident(sym.name.toTermName)
      else
        Select(getPackage(sym.owner), sym.name.toTermName)

    def isMemberOfObject(m: Symbol) = TypeTag.Object.tpe.member(m.name) != NoSymbol

    //! TODO - This is a hack, but it's unclear what it should be instead. See
    //! https://groups.google.com/d/topic/scala-user/n11V6_zI5go/discussion
    def resolvedType(m: Symbol) =
      m.typeSignatureIn(SuperType(ThisType(typeToMock.typeSymbol), typeToMock))

    val mockName = c.freshName(typeToMock.typeSymbol.name).toTypeName
    val mockPackage = getPackage(typeToMock.typeSymbol.owner)
    val methodsNotInObject =
      typeToMock.members filter (m => m.isMethod && !isMemberOfObject(m)) map (_.asMethod)
    val methodsToMock = methodsNotInObject.filter { m =>
        !m.isConstructor && (!(m.isStable || m.isAccessor) ||
          m.asInstanceOf[reflect.internal.HasFlags].isDeferred) //! TODO - stop using internal if/when this gets into the API
      }.toList
    val forwarders = methodsToMock map { m => forwarderImpl(m) }
    val mocks = methodsToMock map { m => mockMethod(m) }

    val classDef = q"""class $mockName extends ${typeToMock.typeSymbol.name} {
        ..$forwarders
//        val mocks = Array(..$mocks)
      }"""
    println(classDef)

    c.introduceTopLevel(mockPackage.toString, classDef)
    Select(mockPackage, mockName)
  }
}