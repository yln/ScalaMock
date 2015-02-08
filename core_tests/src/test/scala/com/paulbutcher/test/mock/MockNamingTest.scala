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
import org.scalamock.function.MockFunction
import scala.language.reflectiveCalls

class MockNamingTest extends IsolatedSpec {

  val m = mock[TestTrait]("mock")

  behavior of "Mock"

  it should "have a sensible method name when mocking a method without parameters" in {
    m.fake$noParams$0.toString shouldBe "<mock> TestTrait.noParams"
  }

  it should "have a sensible method name when mocking one parameter method" in {
    m.fake$oneParam$0.toString shouldBe "<mock> TestTrait.oneParam"
  }

  it should "have a sensible method name when mocking an operator" in {
    m.fake$$plus$0.toString shouldBe "<mock> TestTrait.$plus" // TODO could be better
  }

  it should "have a sensible method name when mocking curried method" in {
    m.fake$curried$0.toString shouldBe "<mock> TestTrait.curried"
  }

  it should "have a sensible method name when mocking polymorphic method" in {
    m.fake$polymorphic$0.toString shouldBe "<mock> TestTrait.polymorphic[T]"
  }

  it should "have a sensible method name when mocking overloaded method" in {
    m.fake$overloaded$0.toString shouldBe "<mock> TestTrait.overloaded[T]"
    m.fake$overloaded$1.toString shouldBe "<mock> TestTrait.overloaded"
  }

  it should "have a sensible method name when mocking a class" in {
    val myMock = mock[TestClass]
    myMock.fake$m$0.toString shouldBe "<mock-1> TestClass.m"
  }

  it should "have a sensible method name when mocking polymorphic trait" in {
    val myMock = mock[PolymorphicTrait[List[Int]]]
    myMock.fake$method$0.toString shouldBe "<mock-1> PolymorphicTrait[List[Int]].method[U]"
  }

  it should "have a sensible method name when mocking Java polymorphic interface" in {
    val myMock = mock[JavaGenericInterface[List[Int]]]
    myMock.fake$compare$0.toString shouldBe "<mock-1> JavaGenericInterface[List[Int]].compare"
  }

  it can "be named using string literal" in {
    val myMock = mock[TestTrait]("mock name")
    myMock.fake$oneParam$0.toString shouldBe "<mock name> TestTrait.oneParam"
  }

  it should "should have its name evaluated during mock construction" in {
    var prefix = "mock"
    val mocks = for (idx <- 1 to 2) yield mock[TestTrait](prefix + idx)
    prefix = "changed"

    mocks(0).fake$oneParam$0.toString shouldBe "<mock1> TestTrait.oneParam"
    mocks(1).fake$oneParam$0.toString shouldBe "<mock2> TestTrait.oneParam"
  }

  it should "have sensible default name assigned" in {
    val myMock = mock[TestTrait]
    myMock.fake$noParams$0.toString shouldBe "<mock-1> TestTrait.noParams"
  }

  it should "have consistent names of mocked methods" in {
    val myMock = mock[TestTrait]
    myMock.fake$noParams$0.toString shouldBe "<mock-1> TestTrait.noParams"
    myMock.fake$twoParams$0.toString shouldBe "<mock-1> TestTrait.twoParams" // not <mock-2>
  }

  it should "should have differentiating default name assigned" in {
    val myMock1 = mock[TestTrait]
    val myMock2 = mock[TestTrait]
    myMock2.fake$oneParam$0.toString shouldBe "<mock-2> TestTrait.oneParam"
    myMock1.fake$oneParam$0.toString shouldBe "<mock-1> TestTrait.oneParam"
  }
}
