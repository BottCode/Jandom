package it.unipd.jandom.domains.numerical.box

import it.unipd.jandom.domains.{InfInt, IntNumber, PositiveInfinity, NegativeInfinity}
import it.unipd.jandom.domains.numerical.box.Box._
import it.unipd.jandom.domains.numerical.box.BoundedBoxDomainCore._
import org.scalatest.FlatSpec
/**
  * Unit Test - Bounded Box Domain Core
  *
  * @author Mauro Carlin <>
  * @author Mattia Bottaro <>
  */

class BoundedBoxDomainCoreSuite extends FlatSpec {
  val m = IntNumber(-10)
  val n = IntNumber(10)
  val bc = BoundedBoxDomainCore(m,n)

  val Zero = Box.Interval(IntNumber(0), IntNumber(0))
  val One = Box.Interval(IntNumber(1), IntNumber(1))
  val Seven = Box.Interval(IntNumber(7), IntNumber(7))
  val fourTeen = Box.Interval(IntNumber(14), IntNumber(14))
  val Mone = Box.Interval(IntNumber(-1), IntNumber(-1))
  val zeroTen = Box.Interval(IntNumber(0), IntNumber(10))
  val MtenZero = Box.Interval(IntNumber(-10), IntNumber(0))
  val MtenTen = Box.Interval(IntNumber(-10), IntNumber(10))
  val oneInf = Box.Interval(IntNumber(1), PositiveInfinity())
  val MinfMone = Box.Interval(NegativeInfinity(), IntNumber(-1))
  val MinfZero = Box.Interval(NegativeInfinity(), IntNumber(0))
  val nInf = Box.Interval(n, PositiveInfinity())
  val MinfM = Box.Interval(NegativeInfinity(), m)

  "BoundedBoxDomainCore.alpha" should
    " - return the corresponding abstract value" in {
    assert(bc.alpha(1) === One)
    assert(bc.alpha(0) === Zero)
    assert(bc.alpha(15) !== One)
  }

  "BoundedBoxDomainCore.sum" should
    " - return the sum of the two Boxs given as input" in {
    val sevenInf = Box.Interval(IntNumber(7), PositiveInfinity())
    // IntervalBottom
    assert(bc.sum(IntervalBottom, zeroTen) === IntervalBottom)
    assert(bc.sum(IntervalBottom, IntervalTop) === IntervalBottom)
    assert(bc.sum(oneInf, IntervalBottom) === IntervalBottom)
    assert(bc.sum(IntervalTop, IntervalBottom) === IntervalBottom)
    assert(bc.sum(IntervalBottom, IntervalBottom) === IntervalBottom)
    // IntervalTop
    assert(bc.sum(IntervalTop, MtenZero) === IntervalTop)
    assert(bc.sum(Zero, IntervalTop) === IntervalTop)
    assert(bc.sum(IntervalTop, IntervalTop) === IntervalTop)
    // Interval
    assert(bc.sum(One, Zero) === One)
    assert(bc.sum(zeroTen, Seven) === sevenInf)
    assert(bc.sum(Seven, zeroTen) === sevenInf)
    assert(bc.sum(Seven, Seven) === fourTeen)
    assert(bc.sum(fourTeen, zeroTen) === nInf)
    assert(bc.sum(MinfMone, One) === MinfZero)
    assert(bc.sum(Zero, oneInf) === oneInf)
    assert(bc.sum(oneInf, Zero) === oneInf)
    assert(bc.sum(oneInf, MinfMone) === IntervalTop)
    assert(bc.sum(MinfMone, oneInf) === IntervalTop)

  }

  "BoundedBoxDomainCore.inverse" should
    " - return the inverse of the Box given as input" in {
    val elevenTwelve = Box.Interval(IntNumber(11), IntNumber(12))
    // IntervalBottom
    assert(bc.inverse(IntervalBottom) === IntervalBottom)
    // IntervalTop
    assert(bc.inverse(IntervalTop) === IntervalTop)
    // Interval
    assert(bc.inverse(One) === Mone)
    assert(bc.inverse(Mone) === One)
    assert(bc.inverse(zeroTen) === MtenZero)
    assert(bc.inverse(MtenZero) === zeroTen)
    assert(bc.inverse(MtenTen) === MtenTen)
    assert(bc.inverse(MinfMone) === oneInf)
    assert(bc.inverse(oneInf) === MinfMone)
    assert(bc.inverse(elevenTwelve) === MinfM)
  }


  "BoundedBoxDomainCore.mult" should
    " - return the mult of the two Boxs given as input" in {
    val MinfZero = Box.Interval(NegativeInfinity(), IntNumber(0))
    val MhudredHundred = Box.Interval(IntNumber(-100), IntNumber(100))
    // IntervalBottom
    assert(bc.mult(IntervalBottom, One) === IntervalBottom)
    assert(bc.mult(IntervalBottom, IntervalTop) === IntervalBottom)
    assert(bc.mult(MtenTen, IntervalBottom) === IntervalBottom)
    assert(bc.mult(IntervalTop, IntervalBottom) === IntervalBottom)
    assert(bc.mult(IntervalBottom, IntervalBottom) === IntervalBottom)
    // IntervalTop
    assert(bc.mult(IntervalTop, Mone) === IntervalTop)
    assert(bc.mult(Mone, IntervalTop) === IntervalTop)
    assert(bc.mult(IntervalTop, IntervalTop) === IntervalTop)
    // Interval 0
    assert(bc.mult(IntervalTop, Zero) === Zero)
    assert(bc.mult(Zero, IntervalTop) === Zero)
    assert(bc.mult(IntervalBottom, Zero) === IntervalBottom)
    assert(bc.mult(Zero, IntervalBottom) === IntervalBottom)
    assert(bc.mult(Zero, nInf) === Zero)
    assert(bc.mult(nInf, Zero) === Zero)
    assert(bc.mult(One, Zero) === Zero)
    assert(bc.mult(Zero, One) === Zero)
    // Interval
    assert(bc.mult(zeroTen, MtenZero) === MinfZero)
    assert(bc.mult(MtenZero, zeroTen) === MinfZero)
    assert(bc.mult(MinfMone, One) === MinfMone)
    assert(bc.mult(One, MinfMone) === MinfMone)
    assert(bc.mult(MtenTen, MtenTen) === IntervalTop)
    assert(bc.mult(oneInf, MinfMone) === MinfMone)
    assert(bc.mult(MinfMone, oneInf) === MinfMone)
    assert(bc.mult(oneInf, Mone) === MinfMone)
    assert(bc.mult(Mone, oneInf) === MinfMone)
  }

  "BoundedBoxDomainCore.division" should
    " - return the division of the two Boxs given as input" in {
    val twoThree = Box.Interval(IntNumber(2), IntNumber(3))
    val twoThirteen = Box.Interval(IntNumber(2), IntNumber(13))
    val zeroSix = Box.Interval(IntNumber(0), IntNumber(6))
    // IntervalBottom
    assert(bc.division(IntervalBottom, MtenTen) === IntervalBottom)
    assert(bc.division(IntervalBottom, IntervalTop) === IntervalBottom)
    assert(bc.division(MtenTen, IntervalBottom) === IntervalBottom)
    assert(bc.division(IntervalTop, IntervalBottom) === IntervalBottom)
    assert(bc.division(IntervalBottom, IntervalBottom) === IntervalBottom)
    // IntervalTop
    assert(bc.division(IntervalTop, One) === IntervalTop)
    assert(bc.division(One, IntervalTop) === IntervalTop)
    assert(bc.division(IntervalTop, IntervalTop) === IntervalTop)
    // Interval [0,0])
    assert(bc.division(IntervalTop, Zero) === IntervalBottom)
    assert(bc.division(Zero, IntervalTop) === Zero)
    assert(bc.division(Zero, MtenTen) === Zero)
    assert(bc.division(MtenTen, Zero) === IntervalBottom)

    // Interval
    assert(bc.division(fourTeen, One) === fourTeen)
    assert(bc.division(twoThirteen, twoThree) === zeroSix)
  }

  "BoundedBoxDomainCore.remainder" should
    " - return the remainder of the two Boxs given as input" in {
    // IntervalBottom
    /*assert(bc.remainder(IntervalBottom, One) === IntervalBottom)
    assert(bc.remainder(IntervalBottom, IntervalTop) === IntervalBottom)
    assert(bc.remainder(One, IntervalBottom) === IntervalBottom)
    assert(bc.remainder(IntervalTop, IntervalBottom) === IntervalBottom)
    // IntervalTop
    assert(bc.remainder(IntervalTop, MtenTen) === IntervalTop)
    assert(bc.remainder(MtenTen, IntervalTop) === IntervalTop)
    assert(bc.remainder(IntervalTop, IntervalTop) === IntervalTop)
    // Interval(0)
    assert(bc.remainder(IntervalTop, Zero) === IntervalBottom)
    assert(bc.remainder(Zero, IntervalTop) === Zero)
    assert(bc.remainder(Zero, MtenTen) === Zero)
    assert(bc.remainder(MtenTen, Zero) === IntervalBottom)
    assert(bc.remainder(Zero, Zero) === IntervalBottom)
    // Interval
    assert(bc.remainder(MtenTen, MtenTen) === zero)*/
  }

  "BoundedBoxDomainCore.lub" should
    " - return the least upper bound of the two Boxs given as input" in {
    val MinfTen = Box.Interval(NegativeInfinity(), IntNumber(10))
    val zeroOne = Box.Interval(IntNumber(0), IntNumber(1))
    // IntervalBottom
    assert(bc.lub(IntervalBottom, MtenTen) === MtenTen)
    assert(bc.lub(MtenTen, IntervalBottom) === MtenTen)
    assert(bc.lub(IntervalBottom, IntervalBottom) === IntervalBottom)
    // IntervalTop
    assert(bc.lub(IntervalTop, One) === IntervalTop)
    assert(bc.lub(One, IntervalTop) === IntervalTop)
    assert(bc.lub(IntervalTop, IntervalBottom) === IntervalTop)
    assert(bc.lub(IntervalBottom, IntervalTop) === IntervalTop)
    assert(bc.lub(IntervalTop, IntervalTop) === IntervalTop)
    // Interval
    assert(bc.lub(oneInf, MinfMone) === IntervalTop)
    assert(bc.lub(MinfMone, oneInf) === IntervalTop)
    assert(bc.lub(MinfMone, MtenTen) === MinfTen)
    assert(bc.lub(MtenTen, MinfMone) === MinfTen)
    assert(bc.lub(Zero, One) === zeroOne)
    assert(bc.lub(One, Zero) === zeroOne)
  }

  "BoundedBoxDomainCore.glb" should
    " - return the greatest lower bound between the two Boxs given as input" in {
    val MtenMone = Box.Interval(IntNumber(-10), IntNumber(-1))
    // IntervalBottom
    assert(bc.glb(IntervalBottom, MtenTen) === IntervalBottom)
    assert(bc.glb(MtenTen, IntervalBottom) === IntervalBottom)
    assert(bc.glb(IntervalBottom, IntervalBottom) === IntervalBottom)
    // IntervalTop
    assert(bc.glb(IntervalTop, One) === One)
    assert(bc.glb(One, IntervalTop) === One)
    assert(bc.glb(IntervalTop, IntervalBottom) === IntervalBottom)
    assert(bc.glb(IntervalBottom, IntervalTop) === IntervalBottom)
    assert(bc.glb(IntervalTop, IntervalTop) === IntervalTop)
    // Interval
    assert(bc.glb(oneInf, MinfMone) === IntervalBottom)
    assert(bc.glb(MinfMone, oneInf) === IntervalBottom)
    assert(bc.glb(MinfMone, MtenTen) === MtenMone)
    assert(bc.glb(MtenTen, MinfMone) === MtenMone)
    assert(bc.glb(Zero, One) === IntervalBottom)
    assert(bc.glb(One, Zero) === IntervalBottom)
  }

  "BoundedBoxDomainCore.compare" should
    " - return the comparison between the two Boxs given as input" in {
    // IntervalTop
    assert(bc.compare(IntervalTop, IntervalTop) === Option(0))
    assert(bc.compare(IntervalTop, MtenTen) === Option(1))
    assert(bc.compare(IntervalTop, IntervalBottom) === Option(1))
    assert(bc.compare(MtenTen, IntervalTop) === Option(-1))
    assert(bc.compare(IntervalBottom, IntervalTop) === Option(-1))
    // IntervalBottom
    assert(bc.compare(IntervalBottom, IntervalBottom) === Option(0))
    assert(bc.compare(IntervalBottom, MinfMone) === Option(-1))
    assert(bc.compare(MinfMone, IntervalBottom) === Option(1))
    // Interval
    assert(bc.compare(One, Zero) === Option.empty)
    assert(bc.compare(MinfMone, oneInf) === Option.empty)
    assert(bc.compare(MtenTen, MtenTen) === Option(0))
    assert(bc.compare(nInf,nInf) === Option(0))
  }
} // end of BoundedBoxDomainCoreSuite