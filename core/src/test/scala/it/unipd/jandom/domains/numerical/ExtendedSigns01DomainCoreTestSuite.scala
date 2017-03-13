package it.unipd.jandom.domains.numerical

import org.scalatest.FunSuite

/**
  * Note: hereafter, ES01 will stand for ExtendedSigns01Domain.
  * Test class for ES01's core.
  */
class ExtendedSigns01DomainCoreTestSuite extends FunSuite {

  import ExtendedSigns01DomainCore._

  test("toES01") {
    assert(Zero.equals(toSign(0)))
    assert(One.equals(toSign(1)))
    assert(GreaterThanOne.equals(toSign(2)))
    assert(GreaterThanOne.equals(toSign(100)))
    assert(Negative.equals(toSign(-1)))
    assert(Negative.equals(toSign(-1000)))
  }

  test("inverse") {
    assert(ES01Top.equals(inverse(Negative)))
    assert(Zero.equals(inverse(Zero)))
    assert(Negative.equals(inverse(One)))
    assert(Negative.equals(inverse(GreaterThanOne)))
    assert(ES01Top.equals(inverse(ES01Top)))
    assert(ES01Bottom.equals(inverse(ES01Bottom)))
  }

  test("sum") {
    assert(Negative.equals(sum(Negative, Negative)))
    assert(Zero.equals(sum(Zero, Zero)))
    assert(GreaterThanOne.equals(sum(One, One)))
    assert(GreaterThanOne.equals(sum(GreaterThanOne, GreaterThanOne)))
    assert(Negative.equals(sum(Negative, Zero)))
    assert(Negative.equals(sum(Zero, Negative)))
    assert(ES01Top.equals(sum(One, Negative)))
    assert(ES01Top.equals(sum(Negative, One)))
    assert(ES01Top.equals(sum(GreaterThanOne, Negative)))
    assert(ES01Top.equals(sum(Negative, GreaterThanOne)))
    assert(One.equals(sum(Zero, One)))
    assert(One.equals(sum(One, Zero)))
    assert(GreaterThanOne.equals(sum(Zero, GreaterThanOne)))
    assert(GreaterThanOne.equals(sum(GreaterThanOne, Zero)))
    assert(GreaterThanOne.equals(sum(One, GreaterThanOne)))
    assert(GreaterThanOne.equals(sum(GreaterThanOne, One)))
    assert(ES01Top.equals(sum(Negative, ES01Top)))
    assert(ES01Top.equals(sum(Zero, ES01Top)))
    assert(ES01Top.equals(sum(One, ES01Top)))
    assert(ES01Top.equals(sum(GreaterThanOne, ES01Top)))
    assert(ES01Top.equals(sum(ES01Top, Negative)))
    assert(ES01Top.equals(sum(ES01Top, Zero)))
    assert(ES01Top.equals(sum(ES01Top, One)))
    assert(ES01Top.equals(sum(ES01Top, GreaterThanOne)))
    assert(ES01Bottom.equals(sum(Negative, ES01Bottom)))
    assert(ES01Bottom.equals(sum(Zero, ES01Bottom)))
    assert(ES01Bottom.equals(sum(One, ES01Bottom)))
    assert(ES01Bottom.equals(sum(GreaterThanOne, ES01Bottom)))
    assert(ES01Bottom.equals(sum(ES01Bottom, Negative)))
    assert(ES01Bottom.equals(sum(ES01Bottom, Zero)))
    assert(ES01Bottom.equals(sum(ES01Bottom, One)))
    assert(ES01Bottom.equals(sum(ES01Bottom, GreaterThanOne)))
  }

  test("mult") {
    assert(ES01Top.equals(mult(Negative, Negative)))
    assert(Zero.equals(mult(Zero, Zero)))
    assert(One.equals(mult(One, One)))
    assert(GreaterThanOne.equals(mult(GreaterThanOne, GreaterThanOne)))
    assert(Zero.equals(mult(Negative, Zero)))
    assert(Zero.equals(mult(Zero, Negative)))
    assert(Negative.equals(mult(One, Negative)))
    assert(Negative.equals(mult(Negative, One)))
    assert(Negative.equals(mult(GreaterThanOne, Negative)))
    assert(Negative.equals(mult(Negative, GreaterThanOne)))
    assert(Zero.equals(mult(Zero, One)))
    assert(Zero.equals(mult(One, Zero)))
    assert(Zero.equals(mult(Zero, GreaterThanOne)))
    assert(Zero.equals(mult(GreaterThanOne, Zero)))
    assert(GreaterThanOne.equals(mult(One, GreaterThanOne)))
    assert(GreaterThanOne.equals(mult(GreaterThanOne, One)))
    assert(ES01Top.equals(mult(Negative, ES01Top)))
    assert(Zero.equals(mult(Zero, ES01Top)))
    assert(ES01Top.equals(mult(One, ES01Top)))
    assert(ES01Top.equals(mult(GreaterThanOne, ES01Top)))
    assert(ES01Top.equals(mult(ES01Top, Negative)))
    assert(Zero.equals(mult(ES01Top, Zero)))
    assert(ES01Top.equals(mult(ES01Top, One)))
    assert(ES01Top.equals(mult(ES01Top, GreaterThanOne)))
    assert(ES01Bottom.equals(mult(Negative, ES01Bottom)))
    assert(ES01Bottom.equals(mult(Zero, ES01Bottom)))
    assert(ES01Bottom.equals(mult(One, ES01Bottom)))
    assert(ES01Bottom.equals(mult(GreaterThanOne, ES01Bottom)))
    assert(ES01Bottom.equals(mult(ES01Bottom, Negative)))
    assert(ES01Bottom.equals(mult(ES01Bottom, Zero)))
    assert(ES01Bottom.equals(mult(ES01Bottom, One)))
    assert(ES01Bottom.equals(mult(ES01Bottom, GreaterThanOne)))
  }

  test("division") {
    assert(ES01Top.equals(division(Negative, Negative)))
    assert(ES01Bottom.equals(division(Zero, Zero)))
    assert(One.equals(division(One, One)))
    assert(ES01Top.equals(division(GreaterThanOne, GreaterThanOne)))
    assert(ES01Bottom.equals(division(Negative, Zero)))
    assert(Zero.equals(division(Zero, Negative)))
    assert(ES01Top.equals(division(One, Negative)))
    assert(Negative.equals(division(Negative, One)))
    assert(ES01Top.equals(division(GreaterThanOne, Negative)))
    assert(ES01Top.equals(division(Negative, GreaterThanOne)))
    assert(Zero.equals(division(Zero, One)))
    assert(ES01Bottom.equals(division(One, Zero)))
    assert(Zero.equals(division(Zero, GreaterThanOne)))
    assert(ES01Bottom.equals(division(GreaterThanOne, Zero)))
    assert(Zero.equals(division(One, GreaterThanOne)))
    assert(GreaterThanOne.equals(division(GreaterThanOne, One)))
    assert(ES01Top.equals(division(Negative, ES01Top)))
    assert(Zero.equals(division(Zero, ES01Top)))
    assert(ES01Top.equals(division(One, ES01Top)))
    assert(ES01Top.equals(division(GreaterThanOne, ES01Top)))
    assert(ES01Top.equals(division(ES01Top, Negative)))
    assert(ES01Bottom.equals(division(ES01Top, Zero)))
    assert(ES01Top.equals(division(ES01Top, One)))
    assert(ES01Top.equals(division(ES01Top, GreaterThanOne)))
    assert(ES01Bottom.equals(division(Negative, ES01Bottom)))
    assert(ES01Bottom.equals(division(Zero, ES01Bottom)))
    assert(ES01Bottom.equals(division(One, ES01Bottom)))
    assert(ES01Bottom.equals(division(GreaterThanOne, ES01Bottom)))
    assert(ES01Bottom.equals(division(ES01Bottom, Negative)))
    assert(ES01Bottom.equals(division(ES01Bottom, Zero)))
    assert(ES01Bottom.equals(division(ES01Bottom, One)))
    assert(ES01Bottom.equals(division(ES01Bottom, GreaterThanOne)))
  }

}
