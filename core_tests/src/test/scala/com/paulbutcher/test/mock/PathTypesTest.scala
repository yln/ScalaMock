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
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ShouldMatchers, FreeSpec}

import scala.language.reflectiveCalls
import scala.collection.mutable

class PathTypesTest extends FreeSpec with MockFactory with ShouldMatchers {
    
  "Mocks should" - {
    "mock an embeddded trait" in {
      val t = mock[PathTypes]
      val e = mock[t.Embedded]
      t.expects.referencesEmbedded().returning(e)
      assertResult(e) { t.referencesEmbedded }
    }
    
    // Test for issue #60
    "mock a method returning an optional embedded trait" in {
      val t = mock[PathTypes]
      val e = mock[t.Embedded]
      t.expects.optionalEmbedded().returning(None)
      t.expects.optionalEmbedded().returning(Some(e))
      assertResult(None) { t.optionalEmbedded() }
      assertResult(Some(e)) { t.optionalEmbedded() }
    }
    
    "handle projected types correctly" in {
      val t = mock[PathTypes]
      val e = mock[t.Embedded]
      val o = mock[t.ATrait]
      val i = mock[e.ATrait]
      e.expects.innerTraitProjected().returning(i)
      e.expects.outerTraitProjected().returning(o)
      assertResult(o) { e.outerTraitProjected }
      assertResult(i) { e.innerTraitProjected }
    }
    
    "handle path-dependent types correctly" in {
      val t = mock[PathTypes]
      val e = mock[t.Embedded]
      val o = mock[t.ATrait]
      val i = mock[e.ATrait]
      e.expects.innerTrait().returning(i)
      assertResult(i) { e.innerTrait }
      e.expects.outerTrait().returning(o)
      assertResult(o) { e.outerTrait }
    }

     "handle path-dependent polymorphic types correctly" in {
       val m = mock[PolymorphicTrait[String]]
       val e = mock[m.Embedded[Double]]
       val o = mock[m.ATrait[String, Double]]
       val i = mock[e.ATrait[String, Double]]
       e.expects.innerTrait("foo", 1.23).returning(i)
       e.expects.outerTrait("bar", 4.56).returning(o)
       assertResult(o) { e.outerTrait("bar", 4.56) }
       assertResult(i) { e.innerTrait("foo", 1.23) }
     }

     // Test for issue #63
//     "mock a custom map" in withExpectations {
//       class MapProxy[A, B](map: mutable.Map[A, B]) extends
//         mutable.Map[A, B] with mutable.MapLike[A, B, mutable.Map[A, B]]
//       class MapProxyTestable extends MapProxy(mutable.Map.empty[Int, Char])
//       
//       val mapStub = stub[MapProxyTestable]
//     }
   }
}
