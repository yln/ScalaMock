package com.paulbutcher.test.mock

import com.paulbutcher.test.TestTrait
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.SeqMockFactory
import org.scalatest.{FlatSpec, OneInstancePerTest}

class SeqMockFactoryTest extends FlatSpec with SeqMockFactory with Matchers with OneInstancePerTest {

  behavior of "Sequential mock factory"

  val m = mock[TestTrait]
  (m.oneParam _).expects(1)
  (m.oneParam _).expects(2)

  it should "work with sequential ordering" in {
    m.oneParam(1)
    m.oneParam(2)
  }

  it should "throw with non-sequential ordering" in {
    assertThrows[ExpectationException](withExpectations {
      m.oneParam(2)
    })
  }
}
