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

package com.paulbutcher.test.matchers

import com.paulbutcher.test._
import org.scalamock.matchers.Matcher

/** Example Matcher */
case class UserMatcher(name: Option[String] = None) extends Matcher[User] {
  def withName(name: String): UserMatcher = copy(name = Some(name))
  override def toString() = "UserMatcher(name=%s)".format(name.getOrElse(""))

  override def safeEquals(that: User): Boolean = {
    name.forall(expectedName => that.name == expectedName)
  }
}

class MatchersTest extends IsolatedSpec {

  val mockedMultiplication = mockFunction[Double, Double, Double]
  val testMock = mock[TestTrait]
  val userDatabaseMock = mock[UserDatabase]

  behavior of "MatchEpsilon"

  it should "match anything that's close to the given value" in {
    mockedMultiplication.expects(~5.0, ~10.0)
    mockedMultiplication(5.0001, 9.9999)
  }

  it should "not match anything that's not close enough" in {
    demandExpectationException {
      mockedMultiplication.expects(~5.0, ~10.0)
      mockedMultiplication(5.1, 9.9)
    }
  }

  behavior of "MatchAny"

  it should "match anything" in {
    (testMock.polymorphic _).expects(*).repeat(3)

    testMock.polymorphic(List("55"))
    testMock.polymorphic(List(1, 2, 3))
    testMock.polymorphic(null)
  }

  behavior of "where matcher"

  it can "be used to create complex predicates (one parameter)" in {
    (userDatabaseMock.storeUser _).expects(where { user: User => user.age > 18 && user.name.startsWith("A") }).returning("matched").twice
    (userDatabaseMock.storeUser _).expects(*).returning("unmatched").once

    userDatabaseMock.storeUser(User("Adam", 22)) shouldBe "matched"
    userDatabaseMock.storeUser(User("Eve", 21)) shouldBe "unmatched"
    userDatabaseMock.storeUser(User("Anna", 21)) shouldBe "matched"
  }

  it can "be used to create complex predicates (two parameters)" in {
    (testMock.twoParams _).expects(where { (x, y) => x + y > 100 }).returning("matched").twice
    (testMock.twoParams _).expects(*, *).returning("unmatched").once

    testMock.twoParams(99, 2.0) shouldBe "matched"
    testMock.twoParams(50, 49.0) shouldBe "unmatched"
    testMock.twoParams(50, 51.0) shouldBe "matched"
  }

  behavior of "argThat matcher"

  it can "be used to create complex predicates" in {
    (userDatabaseMock.addUserAddress _)
      .expects(*, argThat { address: Address => address.city == "Berlin" })
      .returning("matched")
    (userDatabaseMock.addUserAddress _)
      .expects(*, argThat[Address] { address => address.city == "London" })
      .returning("matched")
    (userDatabaseMock.addUserAddress _).expects(*, *).returning("unmatched")

    userDatabaseMock.addUserAddress(User("John", 23), Address("Berlin", "Turmstrasse 12")) shouldBe "matched"
    userDatabaseMock.addUserAddress(User("John", 23), Address("Warsaw", "Marszalkowska 123")) shouldBe "unmatched"
    userDatabaseMock.addUserAddress(User("John", 23), Address("London", "Baker Street 221b")) shouldBe "matched"
  }

  it should "be displayed correctly" in {
    val expectation = (userDatabaseMock.addUserAddress _) expects (*, argThat[Address] { _ => true }) never ()
    expectation.toString() should include("UserDatabase.addUserAddress(*, <matcher>)")
  }

  behavior of "custom matcher"

  it can "be used to create complex predicates" in {
    (userDatabaseMock.addUserAddress _) expects (UserMatcher().withName("Alan"), *) returning "matched"
    (userDatabaseMock.addUserAddress _) expects (UserMatcher().withName("Bob"), *) returning "matched"
    (userDatabaseMock.addUserAddress _) expects (*, *) returning "unmatched"

    userDatabaseMock.addUserAddress(User("Alan", 23), Address("Berlin", "Turmstrasse 12")) shouldBe "matched"
    userDatabaseMock.addUserAddress(User("Craig", 23), Address("Warsaw", "Marszalkowska 123")) shouldBe "unmatched"
    userDatabaseMock.addUserAddress(User("Bob", 23), Address("London", "Baker Street 221b")) shouldBe "matched"
  }

  it should "be displayed correctly" in {
    val expectation = (userDatabaseMock.addUserAddress _) expects (UserMatcher().withName("Alan"), *) never ()
    expectation.toString() should include("UserDatabase.addUserAddress(UserMatcher(name=Alan), *)")
  }

  override def newInstance = new MatchersTest
}
