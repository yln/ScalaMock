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

package com.paulbutcher.test

import org.scalamock.scalatest.PowerMockFactory
import org.scalatest.FreeSpec
import org.scalamock._

class MockObjectTest extends FreeSpec with PowerMockFactory {
  
  autoVerify = false
  
  "Mock objects should" - {
    "fail if expectations are not set" in {
      withExpectations {
        val m = mockObject(TestObject)
//      (TestObject.m _).expects(42, "foo").returning("it worked")
//      expect("it worked") { TestObject.m(42, "foo") }
        TestObject.m(42, "foo")
      }
    }
    
    "succeed if expectations are met" in {
      withExpectations {
        val m = mockObject(TestObject)
      }
    }
  }
}