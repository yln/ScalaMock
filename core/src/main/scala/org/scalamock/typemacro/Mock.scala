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

trait Mock {
  import language.experimental.macros

  type mock[T] = macro MockImpl.mock[T]
}

object MockImpl {
  import reflect.macros.Context

  def mock[T: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val typeToMock = weakTypeOf[T]
    val sym = typeToMock.typeSymbol
    val mockName = c.freshName(sym.name).toTypeName
    // val classDef = q"class $mockName extends ${sym.name} { def oneParam(x: Int) = x.toString }"
    val classDef = q"class $mockName { def oneParam(x: Int) = x.toString }"

    def getPackage(sym: Symbol): RefTree = 
      if (sym.owner.name.toString == "<root>")
        Ident(sym.name.toTermName)
      else
        Select(getPackage(sym.owner), sym.name.toTermName)

    val mockPackage = getPackage(sym.owner)

    c.introduceTopLevel(PackageDef(mockPackage, List(classDef)))
    Select(mockPackage, mockName)
  }
}