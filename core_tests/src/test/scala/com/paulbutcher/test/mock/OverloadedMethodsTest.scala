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

package com.paulbutcher.test.mock

import com.paulbutcher.test._
import scala.language.reflectiveCalls

class OverloadedMethodsTest extends IsolatedSpec {

  behavior of "Mocks"

  they should "mock traits with overloaded methods which have different number of type params" in { // test for issue #85
    trait Foo {
      def overloaded[T](x: T): String
      def overloaded(x: String): String
    }

    val fooMock = mock[Foo]
    fooMock.expects.overloaded(1.0) returning "one"
    fooMock.overloaded(1.0) shouldBe "one"

    fooMock.expects.overloaded("2") returning "two"
    fooMock.overloaded("2") shouldBe "two"
  }

   they should "mock traits with overloaded methods which have different number of type params (2)" in {
     trait Foo {
       def overloaded[T](x: T): String
       def overloaded[T](x: T, y: String): String
     }

     val fooMock = mock[Foo]

     fooMock.expects.overloaded(1.0) returning "one"
     fooMock.overloaded(1.0) shouldBe "one"

     fooMock.expects.overloaded(2.0, "foo") returning "two"
     fooMock.overloaded(2.0, "foo") shouldBe "two"
   }

   they should "mock traits with overloaded methods which have different number of type params (3)" in {
     trait Foo {
       def overloaded[T](x: T): String
       def overloaded[T, U](x: T, y: U): String
     }

     val fooMock = mock[Foo]

     fooMock.expects.overloaded(1.0) returning "one"
     fooMock.overloaded(1.0) shouldBe "one"

     fooMock.expects.overloaded(2.0, "foo") returning "two"
     fooMock.overloaded(2.0, "foo") shouldBe "two"
   }

   they should "mock traits with overloaded methods which have different number of type params (4)" in {
     trait Foo {
       def overloaded[T](x: T, y: String): String
       def overloaded[T, U](x: T, y: U): String
     }

     val fooMock = mock[Foo]

     fooMock.expects.overloaded(1.0, "foo") returning "one"
     fooMock.overloaded(1.0, "foo") shouldBe "one"

     fooMock.expects.overloaded("foo", 2.0) returning "two"
     fooMock.overloaded("foo", 2.0) shouldBe "two"
   }

   they should "cope with overloaded methods" in {
     val m = mock[TestTrait]
     m.expects.overloaded(10).returning("got an integer")
     m.expects.overloaded(10, 1.23).returning("got two parameters")
     assertResult("got an integer") { m.overloaded(10) }
     assertResult("got two parameters") { m.overloaded(10, 1.23) }
   }

   they should "cope with polymorphic overloaded methods" in {
     val m = mock[TestTrait]
     m.expects.overloaded(1.23).returning("polymorphic method called")
     assertResult("polymorphic method called") { m.overloaded(1.23) }
   }

   they should "choose between polymorphic and non-polymorphic overloaded methods correctly" in {
     val m = mock[TestTrait]
     m.expects.overloaded(42).returning("non-polymorphic called")
     m.expects.overloaded[Int](42).returning("polymorphic called")
     assertResult("non-polymorphic called") { m.overloaded(42) }
     assertResult("polymorphic called") { m.overloaded[Int](42) }
   }

//  they should "mock PrintStream.print(String)" in { // test for issue #39
//    import java.io.{ OutputStream, PrintStream }
//    class MockablePrintStream extends PrintStream(mock[OutputStream], false)
//
//    val m = mock[MockablePrintStream]
//    m.expects.print("foo")
//    m.print("foo")
//  }
//
  // Test for issue #94
  they should "cope with an overloaded method with a generic list" in {
    trait Foo {
      def overloaded(x: List[_]): String
      def overloaded[T](x: List[T], y: String): String
    }
    
    val fooMock = mock[Foo]
    fooMock.expects.overloaded(List(1.0, 2.0)) returning "one two"
    
    assertResult("one two") { fooMock.overloaded(List(1.0, 2.0)) }
  }
}
