package it.unipd.jandom.domains.numerical


import it.unipd.jandom.domains.numerical.constant.ConstantDomainCore._
import it.unipd.jandom.domains.numerical.constant.Constant._
import org.scalatest.FlatSpec

/**
  * Unit Test - Constant Domain Core
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>, Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *           Stefano Munari <stefano.munari.1@studenti.unipd.it>
  *
  *
  */

class ConstantDomainCoreSuite extends FlatSpec {

  val six = Const(6)
  val zero = Const(0)

  "ConstantDomain.inverse" should
    " - return the inverse of the constant value given as input" in {

    val minusSix = Const(-6)

    assert(inverse(ConstantTop) === ConstantTop)
    assert(inverse(ConstantBottom) === ConstantBottom)
    assert(inverse(six) === minusSix)
    // corner case
    assert(inverse(zero) === zero)
  }

  "ConstantDomain.lub" should
    " - return the lub of the constant values given as input" in {
    // ConstantTop
    assert(lub(ConstantTop, six) === ConstantTop)
    assert(lub(six, ConstantTop) === ConstantTop)
    assert(lub(ConstantTop, ConstantBottom) === ConstantTop)
    assert(lub(ConstantBottom, ConstantTop) === ConstantTop)
    assert(lub(ConstantTop, ConstantTop) === ConstantTop)
    // ConstantBottom
    assert(lub(ConstantBottom, six) === six)
    assert(lub(six, ConstantBottom) === six)
    assert(lub(ConstantBottom, ConstantBottom) === ConstantBottom)
    // Const
    assert(lub(six, zero) === ConstantTop)
    assert(lub(zero, six) === ConstantTop)
    assert(lub(zero, zero) === zero)
  }

  "ConstantDomain.glb" should
    " - return the glb of the constant values given as input" in {
    // ConstantBottom
    assert(glb(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(glb(ConstantBottom, six) === ConstantBottom)
    assert(glb(ConstantTop, ConstantBottom) === ConstantBottom)
    assert(glb(six, ConstantBottom) === ConstantBottom)
    assert(glb(ConstantBottom, ConstantBottom) === ConstantBottom)
    // ConstantTop
    assert(glb(ConstantTop, six) === six)
    assert(glb(six, ConstantTop) === six)
    assert(glb(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(glb(six, zero) === ConstantBottom)
    assert(glb(zero, zero) === zero)
  }

  "ConstantDomain.sum" should
    " - return the sum of the constant values given as input" in {
    // ConstantBottom
    assert(sum(ConstantBottom, six) === ConstantBottom)
    assert(sum(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(sum(six, ConstantBottom) === ConstantBottom)
    assert(sum(ConstantTop, ConstantBottom) === ConstantBottom)
    assert(sum(ConstantBottom, ConstantBottom) === ConstantBottom)
    // ConstantTop
    assert(sum(ConstantTop, six) === ConstantTop)
    assert(sum(six, ConstantTop) === ConstantTop)
    assert(sum(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(sum(six, zero) === six)
  }

  "ConstantDomain.mult" should
    " - return the product of the constant values given as input" in {

    val multPositive=
      six match {
        case Const(value) => Const(value*value)
      }

    // ConstantBottom
    assert(mult(ConstantBottom, six) === ConstantBottom)
    assert(mult(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(mult(six, ConstantBottom) === ConstantBottom)
    assert(mult(ConstantTop, ConstantBottom) === ConstantBottom)
    // Const(0)
    assert(mult(ConstantTop, zero) === zero)
    assert(mult(zero, ConstantTop) === zero)
    assert(mult(zero, six) === zero)
    assert(mult(six, zero) === zero)
    assert(mult(zero, zero) === zero)
    // ConstantTop
    assert(mult(ConstantTop, six) === ConstantTop)
    assert(mult(six, ConstantTop) === ConstantTop)
    assert(mult(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(mult(six, six) === multPositive)
  }

  "ConstantDomain.division" should
    " - return the division of the constant values given as input" in {

    val divisionPositive=
      six match {
        case Const(value) => Const(value/value)
      }

    // ConstantBottom
    assert(division(ConstantBottom, six) === ConstantBottom)
    assert(division(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(division(six, ConstantBottom) === ConstantBottom)
    assert(division(ConstantTop, ConstantBottom) === ConstantBottom)
    // Const(0)
    assert(division(ConstantTop, zero) === ConstantBottom)
    assert(division(zero, ConstantTop) === zero)
    assert(division(zero, six) === zero)
    assert(division(six, zero) === ConstantBottom)
    assert(division(zero, zero) === ConstantBottom)
    // ConstantTop
    assert(division(ConstantTop, six) === ConstantTop)
    assert(division(six, ConstantTop) === ConstantTop)
    assert(division(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(division(six, six) === divisionPositive)
  }

  "ConstantDomain.remainder" should
    " - return the remainder (modulo) of the constant values given as input" in
    {
      // ConstantBottom
      assert(remainder(ConstantBottom, six) === ConstantBottom)
      assert(remainder(ConstantBottom, ConstantTop) === ConstantBottom)
      assert(remainder(six, ConstantBottom) === ConstantBottom)
      assert(remainder(ConstantTop, ConstantBottom) === ConstantBottom)
      // Const(0)
      assert(remainder(ConstantTop, zero) === ConstantBottom)
      assert(remainder(zero, ConstantTop) === zero)
      assert(remainder(zero, six) === zero)
      assert(remainder(six, zero) === ConstantBottom)
      assert(remainder(zero, zero) === ConstantBottom)
      // ConstantTop
      assert(remainder(ConstantTop, six) === ConstantTop)
      assert(remainder(six, ConstantTop) === ConstantTop)
      assert(remainder(ConstantTop, ConstantTop) === ConstantTop)
      // Const
      assert(remainder(six, six) === zero)
    }

  "ConstantDomain.compare" should
    " - return the comparison between the constant values given as input" in {
    // ConstantTop
    assert(compare(ConstantTop, ConstantTop) === Option(0))
    assert(compare(ConstantTop, six) === Option(1))
    assert(compare(ConstantTop, ConstantBottom) === Option(1))
    assert(compare(six, ConstantTop) === Option(-1))
    assert(compare(ConstantBottom, ConstantTop) === Option(-1))
    // ConstantBottom
    assert(compare(ConstantBottom, ConstantBottom) === Option(0))
    assert(compare(ConstantBottom, ConstantTop) === Option(-1))
    assert(compare(ConstantBottom, six) === Option(-1))
    assert(compare(ConstantTop, ConstantBottom) === Option(1))
    assert(compare(six, ConstantBottom) === Option(1))
    // Const
    assert(compare(six, zero) === Option.empty)
    assert(compare(six, six) === Option(0))
  }
}