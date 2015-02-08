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

class JavaMocksTest extends IsolatedSpec {
  behavior of "ScalaMock while mocking Java classes and interfaces"

  it should "mock Java generics" in {
    val m = mock[JavaGenericInterface[Int]]
    m.expects.simpleMethod("two") returning 42

    m.simpleMethod("two") shouldBe 42
  }

  it should "mock classes with bridged methods" in {
    val m = mock[JavaClassWithBridgeMethod]

    m.expects.compare(new Integer(5)).returning(1)
    m.expects.compare(new Integer(6)).returning(2)

    def useBridgeMethod[T](gen: JavaGenericInterface[T], x: T) = {
      gen.compare(x)
    }

    assertResult(1) { m.compare(new Integer(5)) } // calls: int compare(Integer)
    assertResult(2) { useBridgeMethod(m, new Integer(6)) } // calls: int compare(Object)
  }

  it should "cope with Java methods with repeated parameters" in {
    val m = mock[JavaInterface]
    m.expects.repeatedParam(42, Seq(1.23, 4.56))
    m.repeatedParam(42, 1.23, 4.56)
  }

  it should "mock a Java interface" in {
    val m = mock[JavaInterface]
    m.expects.m(42, "foo").returning("a return value")
    assertResult("a return value") { m.m(42, "foo") }
  }

  it should "mock a Polymorhpic Java interface" in { // test for issue #24
    val m = mock[PolymorphicJavaInterface]
    m.expects.simplePolymorphicMethod("foo").returning(44)
    assertResult(44) { m.simplePolymorphicMethod("foo") }
  }

  it should "mock a Polymorhpic Java interface (type parametrized method parameter)" in {
    val m = mock[PolymorphicJavaInterface]
    val arg = new java.util.ArrayList[String]
    m.expects.polymorphicMethod[String](arg).returning("foo")

    m.polymorphicMethod(arg) shouldBe "foo"
  }

  it should "mock a Java class with an overloaded method (different param count)" in { // test for issue #34
    val m = mock[JavaClassWithOverloadedMethod]
    m.expects.overloadedMethod("a").returning("first")
    m.expects.overloadedMethod("a", "b").returning("second")

    m.overloadedMethod("a") shouldBe "first"
    m.overloadedMethod("a", "b") shouldBe "second"
  }

  it should "mock a Java class with an overloaded method (the same param count)" in { // test for issue #73
    val m = mock[JavaClassWithOverloadedMethod]
    m.expects.overloadedSameParamCount("one").returning("first")
    m.expects.overloadedSameParamCount(new Integer(2)).returning(2)

    m.overloadedSameParamCount("one") shouldBe "first"
    m.overloadedSameParamCount(2) shouldBe 2
  }

  it should "mock a Java class with an overloaded method (with primitive param)" in { // test for issue #73
    val m = mock[JavaClassWithOverloadedMethod]
    m.expects.overloadedWithPrimitiveParam("one").returning("first")
    m.expects.overloadedWithPrimitiveParam(2).returning("second")

    m.overloadedWithPrimitiveParam("one") shouldBe "first"
    m.overloadedWithPrimitiveParam(2) shouldBe "second"
  }

  it should "mock a Java class with an overloaded method (with type params)" in {
    val m = mock[JavaClassWithOverloadedMethod]
    m.expects.overloadedGeneric("one").returning("first")
    m.expects.overloadedGeneric(2).returning("second")

    m.overloadedGeneric("one") shouldBe "first"
    m.overloadedGeneric(2) shouldBe "second"
  }
  
  //! TODO Test for issue #86
//  it should "mock a Java method taking an unrefined Map" in {
//    val m = mock[Issue86]
//  }
}
