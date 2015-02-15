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

package com.paulbutcher.test

trait PathTypes {
  trait Embedded {
//    def m(x: Int, y: Double): String
//
//    trait ATrait
//    def innerTrait(): ATrait
//    def outerTrait(): PathTypes.this.ATrait
//    def innerTraitProjected(): PathTypes#Embedded#ATrait
//    def outerTraitProjected(): PathTypes#ATrait

    //! TODO - we should be able to handle arguments of these types as well as return values
//    def innerTrait(x: ATrait): ATrait
//    def outerTrait(x: PathTypes.this.ATrait): PathTypes.this.ATrait
//    def innerTraitProjected(x: PathTypes#Embedded#ATrait): PathTypes#Embedded#ATrait
//    def outerTraitProjected(x: PathTypes#ATrait): PathTypes#ATrait
  }
  
//  trait ATrait
  
  def referencesEmbedded(): Embedded
//  def optionalEmbedded(): Option[Embedded]
  
  //! TODO - handle this.type
//  def returnsThisType(): this.type
}