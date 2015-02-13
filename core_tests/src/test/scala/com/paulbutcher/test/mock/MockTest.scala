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
import org.scalamock.function.FunctionAdapter1
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ShouldMatchers, FreeSpec}
import some.other.pkg._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{ TypeTag, typeTag }
import scala.util.{Try, Failure}
import scala.language.reflectiveCalls
import scala.collection.mutable

class MockTest extends FreeSpec with MockFactory with ShouldMatchers {
  
  autoVerify = false
  
   "Mocks should" - {
     "fail if an unexpected method call is made" in {
       withExpectations {
         val m = mock[TestTrait]
         intercept[ExpectationException] { m.oneParam(42) }
       }
     }
    
     "allow expectations to be set" in {
       withExpectations {
         val m = mock[TestTrait]
         m.expects.twoParams(42, 1.23).returning("a return value")
         assertResult("a return value") { m.twoParams(42, 1.23) }
       }
     }
    
     "fail if a non-matching method call is made" in {
       withExpectations {
         val m = mock[TestTrait]
         m.expects.twoParams(42, 1.23)
         intercept[ExpectationException] { m.twoParams(1, 1.0) }
         m.twoParams(42, 1.23)
       }
     }

     "cope with nullary methods" in {
       withExpectations {
         val m = mock[TestTrait]
         m.expects.nullary.returning("a return value")
         assertResult("a return value") { m.nullary }
       }
     }

     "cope with infix operators" in {
       withExpectations {
         val m1 = mock[TestTrait]
         val m2 = mock[TestTrait]
         val m3 = mock[TestTrait]
         m1.expects.+(m2).returning(m3)
         assertResult(m3) { m1 + m2 }
       }
     }
    
     "cope with curried methods" in {
       withExpectations {
         val m = mock[TestTrait]
         m.expects.curried(10)(1.23).returning("curried method called")
         val partial = m.curried(10) _
         assertResult("curried method called") { partial(1.23) }
       }
     }
    
     "cope with polymorphic methods" in {
       withExpectations {
         val m = mock[TestTrait]
         m.expects.polymorphic(List(1, 2)).returning("called with integers")
         m.expects.polymorphic(List("foo", "bar")).returning("called with strings")
         assertResult("called with integers") { m.polymorphic(List(1, 2)) }
         assertResult("called with strings") { m.polymorphic(List("foo", "bar")) }
       }
     }
    
     "cope with curried polymorphic methods" in {
       withExpectations {
         val m = mock[TestTrait]
         m.expects.polycurried(42)("foo").returning((123, "bar"))
         val partial = m.polycurried(42)(_: String)
         assertResult((123, "bar")) { partial("foo") }
       }
     }
    
     "cope with parameters of polymorphic type" in {
       withExpectations {
         val m = mock[TestTrait]
         m.expects.polymorphicParam(42, 1.23).returning("it works")
         assertResult("it works") { m.polymorphicParam((42, 1.23)) }
       }
     }

     "cope with methods with repeated parameters" in {
       withExpectations {
         val m = mock[TestTrait]
         m.expects.repeatedParam(42, List("foo", "bar"))
         m.repeatedParam(42, "foo", "bar")
       }
     }
    
     "cope with methods where Seq[T] is the last parameter" in { // issue #54
       trait ClassWithSeqTParam {
          def run(ints: Seq[Int]) : Int
       }

       withExpectations {
         val m = mock[ClassWithSeqTParam]

         m.expects.run(Seq(1, 2, 3)).returning(100)
         assertResult(100) { m.run(Seq(1, 2, 3)) }

         m.expects.run(*).returning(200)
         assertResult(200) { m.run(Seq(5, 55)) }

         m.expects.run(Seq()).returning(300)
         assertResult(300) { m.run(Seq.empty) }
       }
     }
    
     "cope with methods with by name parameters" in {
       withExpectations {
         val m = mock[TestTrait]
         m.expects.byNameParam(*).returning("it worked")
         assertResult("it worked") { m.byNameParam(42) }
       }
     }
    
//     //! TODO - find a way to make this less ugly
//     "match methods with by name parameters" in {
//       withExpectations {
//         val m = mock[TestTrait]
//         val f: (=> Int) => Boolean = { x => x == 1 && x == 2  }
//         ((m.byNameParam _): (=> Int) => String).expects(new FunctionAdapter1(f)).returning("it works")
//         var y = 0
//         assertResult("it works") { m.byNameParam { y += 1; y } }
//       }
//     }
    
     "cope with methods with implicit parameters" in {
       withExpectations {
         implicit val y: Double = 1.23
         val m = mock[TestTrait]
         m.expects.implicitParam(42)(1.23).returning("it works")
         assertResult("it works") { m.implicitParam(42) }
       }
     }

     "cope with references to another package" in {
       withExpectations {
         val m = mock[TestTrait]
         val x = new SomeOtherClass
         m.expects.referencesSomeOtherPackage(x).returning(x)
         assertResult(x) { m.referencesSomeOtherPackage(x) }
       }
     }

     "cope with upper bound in another package" in {
       withExpectations {
         val m = mock[TestTrait]
         val x = new SomeOtherClass
         m.expects.otherPackageUpperBound(x).returning(x)
         assertResult(x) { m.otherPackageUpperBound(x) }
       }
     }
    
     "cope with explicit references to another package" in {
       withExpectations {
         val m = mock[TestTrait]
         val x = new yet.another.pkg.YetAnotherClass
         m.expects.explicitPackageReference(x).returning(x)
         assertResult(x) { m.explicitPackageReference(x) }
       }
     }

     "cope with upper bound in an explictly referenced package" in {
       withExpectations {
         val m = mock[TestTrait]
         val x = new yet.another.pkg.YetAnotherClass
         m.expects.explicitPackageUpperBound(x).returning(x)
         assertResult(x) { m.explicitPackageUpperBound(x) }
       }
     }

     "cope with a var" in {
       withExpectations {
         val m = mock[TestTrait]
         m.expects.aVar_=("foo")
         m.expects.aVar.returning("bar")
         m.aVar = "foo"
         assertResult("bar") { m.aVar }
       }
     }
    
//     //! TODO - currently doesn't work because we can't override concrete vars
//     "cope with a non-abstract var" ignore {
//       withExpectations {
//         val m = mock[TestTrait]
//         (m.concreteVar_= _).expects("foo")
//         (m.concreteVar _).expects().returning("bar")
//         m.concreteVar = "foo"
//         assertResult("bar") { m.concreteVar }
//       }
//     }
    
    "cope with a val" in {
      withExpectations {
        val m = mock[TestTrait]
        assertResult(null) { m.aVal }
      }
    }
    
    "cope with a non-abstract val" in {
      withExpectations {
        val m = mock[TestTrait]
        assertResult("foo") { m.concreteVal }
      }
    }
    
    "cope with a function val" in {
      withExpectations {
        val m = mock[TestTrait]
        assertResult(null) { m.fnVal }
      }
    }
    
    "mock a lazy val" in withExpectations {
      val m = mock[TestTrait]
      assertResult("foo") { m.lazyVal }
    }
    
    "cope with non-abstract methods" in {
      withExpectations {
        val m = mock[TestTrait]
        m.expects.withImplementation(42).returning(1234)
        assertResult(1234) { m.withImplementation(42) }
      }
    }
    
//     "cope with upper bounds" in {
//       withExpectations {
//         val m = mock[TestTrait]
//         m.expects.upperBound((42, "foo")).returning(2)
//         assertResult(2) { m.upperBound((42, "foo")) }
//       }
//     }
//    
//     "cope with lower bounds" in {
//       withExpectations {
//         val m = mock[TestTrait]
//         m.expects.lowerBound((1, 2), List[Product]()).returning("it works")
//         assertResult("it works") { m.lowerBound((1, 2), List[Product]()) }
//       }
//     }
//    
//     "cope with context bounds" in {
//       withExpectations {
//         val m = mock[TestTrait]
//         m.expects.contextBound("foo")(typeTag[java.lang.String]).returning("it works")
//         assertResult("it works") { m.contextBound("foo") }
//       }
//     }
//    
//     "cope with view bounds" in {
//       withExpectations {
//         val m = mock[TestTrait]
//         m.expects.viewBound(1, 2)(*).returning(true)
//         assertResult(true) { m.viewBound(1, 2) }
//       }
//     }
//    
//     "mock a polymorphic trait" in {
//       withExpectations {
//         val m = mock[PolymorphicTrait[String]]
//         m.expects.method(42, "foo", 1.23).returning("a return value")
//         assertResult("a return value") { m.method(42, "foo", 1.23) }
//       }
//     }
//
//     "handle path-dependent polymorphic types correctly" in {
//       withExpectations {
//         val m = mock[PolymorphicTrait[String]]
//         val e = mock[m.Embedded[Double]]
//         val o = mock[m.ATrait[String, Double]]
//         val i = mock[e.ATrait[String, Double]]
//         e.expects.innerTrait("foo", 1.23).returning(i)
//         e.expects.outerTrait("bar", 4.56).returning(o)
//         assertResult(o) { e.outerTrait("bar", 4.56) }
//         assertResult(i) { e.innerTrait("foo", 1.23) }
//       }
//     }
//
//     "mock a class" in {
//       withExpectations {
//         val m = mock[TestClass]
//         m.expects.m(42, "foo").returning((123, "bar"))
//         assertResult((123, "bar")) { m.m(42, "foo") }
//       }
//     }
//    
//     "mock a specialized class" in {
//       withExpectations {
//         val m1 = mock[SpecializedClass[Int]]
//         m1.expects.identity(42).returning(43)
//         assertResult(43) { m1.identity(42) }
//        
//         val m2 = mock[SpecializedClass[List[String]]]
//         m2.expects.identity(List("one", "two", "three")).returning(List("four", "five", "six"))
//         assertResult(List("four", "five", "six")) { m2.identity(List("one", "two", "three")) }
//       }
//     }
//
//     "mock java.io.File" in {
//        class MyFile extends java.io.File("")
//
//        withExpectations {
//          val m = mock[MyFile]
//        }
//      }
//    
//     "allow to be declared as var" in { // test for issue #62
//       withExpectations {
//         var m = mock[TestTrait]
//         m.expects.oneParam(42).returning("foo")
//         assertResult("foo") { m.oneParam(42) }
//       }
//     }
//
//     "mock Function1[A, B] trait" in withExpectations { // test for issue #69
//       val f = mock[Function1[Any, Boolean]]
//       f.expects.apply(*).returning(true)
//       f("this is something") shouldBe true
//     }
//
//     "mock methods that need a class tag" in withExpectations {
//       case class User(first: String, last: String, enabled: Boolean)
//
//       trait DataProviderComponent {
//         def find[T: ClassTag](id: Int): Try[T]
//       }
//
//       val provider = mock[DataProviderComponent]
//
//       provider.expects.find[User](13)(*) returning (Failure[User](new Exception()))
//       provider.find[User](13) shouldBe a[Failure[_]]
//     }
//     
//     "treat stubs as syntactic sugar for anyNumberOfTimes" in withExpectations {
//       val m = mock[TestTrait]
//       m.stubs.oneParam(*).returning("foo")
//       assertResult("foo") { m.oneParam(1) }
//       assertResult("foo") { m.oneParam(2) }
//       assertResult("foo") { m.oneParam(3) }
//     }
//     
//     // Test for issue #76
//     "mock a trait extending PartiallyOrdered" in withExpectations {
//       trait P extends PartiallyOrdered[P] {
//         def f(x: P): P
//       }
//       val m = mock[P]
//     }
//     
//     // Test for issue #93
//     "mock an overloaded method with a generic parameter available to the trait" in withExpectations {
//       trait OverloadedMultiParams[A] {
//         def meth(i: Int, a: A): Int
//         def meth(): Int
//       }
//      
//       val mockTrait = mock[OverloadedMultiParams[String]]
//       mockTrait.expects.meth(1, "Hello").returns(1)
//       assertResult(1) { mockTrait.meth(1, "Hello") }
//     }
   }
}
