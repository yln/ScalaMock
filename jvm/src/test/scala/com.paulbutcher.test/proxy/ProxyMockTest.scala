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

package com.paulbutcher.test.proxy

import com.paulbutcher.test.TestTrait
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.FreeSpec

class ProxyMockTest extends FreeSpec with MockFactory {
  
  "Mocks should" - {
    "fail if an unexpected method call is made" in {
      val m = mock[TestTrait]
      intercept[ExpectationException] { m.oneParam(42) }
    }

    "allow expectations to be set" in {
      val m = mock[TestTrait]
      m.expects('twoParams)(42, 1.23).returning("a return value")
      assertResult("a return value") { m.twoParams(42, 1.23) }
    }
    
    "fail if a non-matching method call is made" in {
      val m = mock[TestTrait]
      m.expects('twoParams)(42, 1.23)
      intercept[ExpectationException] { m.twoParams(1, 1.0) }
      m.twoParams(42, 1.23)
    }
  
    "cope with nullary methods" in {
      val m = mock[TestTrait]
      m.expects('nullary)().returning("a return value")
      assertResult("a return value") { m.nullary }
    }
    
    "cope with overloaded methods" in {
      val m = mock[TestTrait]
      m.expects('overloaded)(10).returning("got an integer")
      m.expects('overloaded)(10, 1.23).returning("got two parameters")
      assertResult("got an integer") { m.overloaded(10) }
      assertResult("got two parameters") { m.overloaded(10, 1.23) }
    }
    
    "cope with polymorphic overloaded methods" in {
      val m = mock[TestTrait]
      m.expects('overloaded)(1.23).returning("polymorphic method called")
      assertResult("polymorphic method called") { m.overloaded(1.23) }
    }
    
    "cope with infix operators" in {
      val m1 = mock[TestTrait]
      val m2 = mock[TestTrait]
      val m3 = mock[TestTrait]
      m1.expects(Symbol("$plus"))(m2).returning(m3)
      assertResult(m3) { m1 + m2 }
    }
    
    "cope with curried methods" in {
      val m = mock[TestTrait]
      m.expects('curried)(10, 1.23).returning("curried method called")
      val partial = m.curried(10) _
      assertResult("curried method called") { partial(1.23) }
    }
    
    "cope with polymorphic methods" in {
      val m = mock[TestTrait]
      m.expects('polymorphic)(List(1, 2)).returning("called with integers")
      m.expects('polymorphic)(List("foo", "bar")).returning("called with strings")
      assertResult("called with integers") { m.polymorphic(List(1, 2)) }
      assertResult("called with strings") { m.polymorphic(List("foo", "bar")) }
    }
    
    "cope with parameters of polymorphic type" in {
      val m = mock[TestTrait]
      m.expects('polymorphicParam)((42, 1.23)).returning("it works")
      assertResult("it works") { m.polymorphicParam((42, 1.23)) }
    }

    "cope with methods with repeated parameters" in {
      val m = mock[TestTrait]
      m.expects('repeatedParam)(42, Seq("foo", "bar"))
      m.repeatedParam(42, "foo", "bar")
    }
    
    "cope with methods with by name parameters" in {
      val m = mock[TestTrait]
      m.expects('byNameParam)(*).returning("it worked")
      assertResult("it worked") { m.byNameParam(42) }
    }

    "cope with a var" in {
      val m = mock[TestTrait]
      m.expects(Symbol("aVar_$eq"))("foo")
      m.expects('aVar)().returning("bar")
      m.aVar = "foo"
      assertResult("bar") { m.aVar }
    }
    
    "cope with a non-abstract var" in {
      val m = mock[TestTrait]
      m.expects(Symbol("concreteVar_$eq"))("foo")
      m.expects('concreteVar)().returning("bar")
      m.concreteVar = "foo"
      assertResult("bar") { m.concreteVar }
    }
    
    "cope with a val" in {
      val m = mock[TestTrait]
      m.expects('aVal)().returning("it works")
      assertResult("it works") { m.aVal }
    }
    
    "cope with a non-abstract val" in {
      val m = mock[TestTrait]
      m.expects('concreteVal)().returning("it works")
      assertResult("it works") { m.concreteVal }
    }
    
    "cope with non-abstract methods" in {
      val m = mock[TestTrait]
      m.expects('withImplementation)(42).returning(1234)
      assertResult(1234) { m.withImplementation(42) }
    }
    
    "mock an embeddded trait" in {
      val m = mock[TestTrait]
      val e = mock[m.Embedded]
      m.expects('referencesEmbedded)().returning(e)
      assertResult(e) { m.referencesEmbedded }
    }
    
    "handle projected types correctly" in {
      val m = mock[TestTrait]
      val e = mock[m.Embedded]
      val o = mock[m.ATrait]
      val i = mock[e.ATrait]
      e.expects('innerTraitProjected)().returning(i)
      e.expects('outerTraitProjected)().returning(o)
      assertResult(o) { e.outerTraitProjected }
      assertResult(i) { e.innerTraitProjected }
    }
    
    "handle path-dependent types correctly" in {
      val m = mock[TestTrait]
      val e = mock[m.Embedded]
      val o = mock[m.ATrait]
      val i = mock[e.ATrait]
      e.expects('innerTrait)().returning(i)
      e.expects('outerTrait)().returning(o)
      assertResult(o) { e.outerTrait }
      assertResult(i) { e.innerTrait }
    }

    "match arguments" in {
      val m = mock[TestTrait]
      m.expects('twoParams)(where { (x: Int, y: Double) => x < y }).returning("less than")
      m.expects('twoParams)(where { (x: Int, y: Double) => x > y }).returning("greater than")
      assertResult("less than") { m.twoParams(1, 2.0) }
      assertResult("greater than") { m.twoParams(2, 1.0) }
    }
  }

  "Stubs should" - {
    "return null unless told otherwise" in {
      val m = stub[TestTrait]
      assertResult(null) { m.oneParam(42) }
    }

    "return what they're told to" in {
      val m = stub[TestTrait]
      m.when('twoParams)(42, 1.23).returns("a return value")
      assertResult("a return value") { m.twoParams(42, 1.23) }
    }
    
    "verify calls" in {
      val m = stub[TestTrait]
      m.twoParams(42, 1.23)
      m.twoParams(42, 1.23)
      m.verify('twoParams)(42, 1.23).twice
    }
    
    "fail when verification fails" in {
      intercept[ExpectationException](withExpectations {
        val m = stub[TestTrait]
        m.twoParams(42, 1.00)
        m.verify('twoParams)(42, 1.23).once
      })
    }

    "match arguments" in {
      val m = stub[TestTrait]
      m.when('twoParams)(where { (x: Int, y: Double) => x < y }).returns("less than")
      m.when('twoParams)(where { (x: Int, y: Double) => x > y }).returns("greater than")
      assertResult("less than") { m.twoParams(1, 2.0) }
      assertResult("greater than") { m.twoParams(2, 1.0) }
      m.verify('twoParams)(where { (x: Int, y: Double) => x < y }).once
      m.verify('twoParams)(where { (x: Int, y: Double) => x > y }).once
    }
  }
}
