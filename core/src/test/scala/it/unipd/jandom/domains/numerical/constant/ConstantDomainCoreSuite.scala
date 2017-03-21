package it.unipd.jandom.domains.numerical.constant

import it.unipd.jandom.domains.numerical.constant.Constant._
import it.unipd.jandom.domains.numerical.constant.ConstantDomainCore._
import org.scalatest.FlatSpec

/**
  * Unit Test - Constant Domain Core
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ConstantDomainCoreSuite extends FlatSpec {

  val cdc = ConstantDomainCore()
  val six = Const(6)
  val zero = Const(0)

  "ConstantDomainCore.inverse" should
    " - return the inverse of the constant value given as input" in {
    val minusSix = Const(-6)

    assert(cdc.inverse(ConstantTop) === ConstantTop)
    assert(cdc.inverse(ConstantBottom) === ConstantBottom)
    assert(cdc.inverse(six) === minusSix)
    // corner case
    assert(cdc.inverse(zero) === zero)
  }

  "ConstantDomainCore.lub" should
    " - return the lub of the constant values given as input" in {
    // ConstantTop
    assert(cdc.lub(ConstantTop, six) === ConstantTop)
    assert(cdc.lub(six, ConstantTop) === ConstantTop)
    assert(cdc.lub(ConstantTop, ConstantBottom) === ConstantTop)
    assert(cdc.lub(ConstantBottom, ConstantTop) === ConstantTop)
    assert(cdc.lub(ConstantTop, ConstantTop) === ConstantTop)
    // ConstantBottom
    assert(cdc.lub(ConstantBottom, six) === six)
    assert(cdc.lub(six, ConstantBottom) === six)
    assert(cdc.lub(ConstantBottom, ConstantBottom) === ConstantBottom)
    // Const
    assert(cdc.lub(six, zero) === ConstantTop)
    assert(cdc.lub(zero, six) === ConstantTop)
    assert(cdc.lub(zero, zero) === zero)
  }

  "ConstantDomainCore.glb" should
    " - return the glb of the constant values given as input" in {
    // ConstantBottom
    assert(cdc.glb(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(cdc.glb(ConstantBottom, six) === ConstantBottom)
    assert(cdc.glb(ConstantTop, ConstantBottom) === ConstantBottom)
    assert(cdc.glb(six, ConstantBottom) === ConstantBottom)
    assert(cdc.glb(ConstantBottom, ConstantBottom) === ConstantBottom)
    // ConstantTop
    assert(cdc.glb(ConstantTop, six) === six)
    assert(cdc.glb(six, ConstantTop) === six)
    assert(cdc.glb(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(cdc.glb(six, zero) === ConstantBottom)
    assert(cdc.glb(zero, six) === ConstantBottom)
    assert(cdc.glb(zero, zero) === zero)
  }

  "ConstantDomainCore.sum" should
    " - return the sum of the constant values given as input" in {
    val twelve = Const(12)

    // ConstantBottom
    assert(cdc.sum(ConstantBottom, six) === ConstantBottom)
    assert(cdc.sum(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(cdc.sum(six, ConstantBottom) === ConstantBottom)
    assert(cdc.sum(ConstantTop, ConstantBottom) === ConstantBottom)
    assert(cdc.sum(ConstantBottom, ConstantBottom) === ConstantBottom)
    // ConstantTop
    assert(cdc.sum(ConstantTop, six) === ConstantTop)
    assert(cdc.sum(six, ConstantTop) === ConstantTop)
    assert(cdc.sum(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(cdc.sum(six, zero) === six)
    assert(cdc.sum(zero, six) === six)
    assert(cdc.sum(six, six) === twelve)
  }

  "ConstantDomainCore.mult" should
    " - return the product of the constant values given as input" in {
    val multPositive=
      six match {
        case Const(value) => Const(value*value)
      }

    // ConstantBottom
    assert(cdc.mult(ConstantBottom, six) === ConstantBottom)
    assert(cdc.mult(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(cdc.mult(six, ConstantBottom) === ConstantBottom)
    assert(cdc.mult(ConstantTop, ConstantBottom) === ConstantBottom)
    // Const(0)
    assert(cdc.mult(ConstantTop, zero) === zero)
    assert(cdc.mult(zero, ConstantTop) === zero)
    assert(cdc.mult(zero, six) === zero)
    assert(cdc.mult(six, zero) === zero)
    assert(cdc.mult(zero, zero) === zero)
    // ConstantTop
    assert(cdc.mult(ConstantTop, six) === ConstantTop)
    assert(cdc.mult(six, ConstantTop) === ConstantTop)
    assert(cdc.mult(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(cdc.mult(six, six) === multPositive)
  }

  "ConstantDomainCore.division" should
    " - return the division of the constant values given as input" in {
    val one = Const(1)

    // ConstantBottom
    assert(cdc.division(ConstantBottom, six) === ConstantBottom)
    assert(cdc.division(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(cdc.division(six, ConstantBottom) === ConstantBottom)
    assert(cdc.division(ConstantTop, ConstantBottom) === ConstantBottom)
    // Const(0)
    assert(cdc.division(ConstantTop, zero) === ConstantBottom)
    assert(cdc.division(zero, ConstantTop) === zero)
    assert(cdc.division(zero, six) === zero)
    assert(cdc.division(six, zero) === ConstantBottom)
    assert(cdc.division(zero, zero) === ConstantBottom)
    // ConstantTop
    assert(cdc.division(ConstantTop, six) === ConstantTop)
    assert(cdc.division(six, ConstantTop) === ConstantTop)
    assert(cdc.division(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(cdc.division(six, six) === one)
  }

  "ConstantDomainCore.remainder" should
    " - return the remainder (modulo) of the constant values given as input" in
    {
      // ConstantBottom
      assert(cdc.remainder(ConstantBottom, six) === ConstantBottom)
      assert(cdc.remainder(ConstantBottom, ConstantTop) === ConstantBottom)
      assert(cdc.remainder(six, ConstantBottom) === ConstantBottom)
      assert(cdc.remainder(ConstantTop, ConstantBottom) === ConstantBottom)
      // Const(0)
      assert(cdc.remainder(ConstantTop, zero) === ConstantBottom)
      assert(cdc.remainder(zero, ConstantTop) === zero)
      assert(cdc.remainder(zero, six) === zero)
      assert(cdc.remainder(six, zero) === ConstantBottom)
      assert(cdc.remainder(zero, zero) === ConstantBottom)
      // ConstantTop
      assert(cdc.remainder(ConstantTop, six) === ConstantTop)
      assert(cdc.remainder(six, ConstantTop) === ConstantTop)
      assert(cdc.remainder(ConstantTop, ConstantTop) === ConstantTop)
      // Const
      assert(cdc.remainder(six, six) === zero)
    }

  "ConstantDomainCore.compare" should
    " - return the comparison between the constant values given as input" in {
    // ConstantTop
    assert(cdc.compare(ConstantTop, ConstantTop) === Option(0))
    assert(cdc.compare(ConstantTop, six) === Option(1))
    assert(cdc.compare(ConstantTop, ConstantBottom) === Option(1))
    assert(cdc.compare(six, ConstantTop) === Option(-1))
    assert(cdc.compare(ConstantBottom, ConstantTop) === Option(-1))
    // ConstantBottom
    assert(cdc.compare(ConstantBottom, ConstantBottom) === Option(0))
    assert(cdc.compare(ConstantBottom, ConstantTop) === Option(-1))
    assert(cdc.compare(ConstantBottom, six) === Option(-1))
    assert(cdc.compare(ConstantTop, ConstantBottom) === Option(1))
    assert(cdc.compare(six, ConstantBottom) === Option(1))
    // Const
    assert(cdc.compare(six, zero) === Option.empty)
    assert(cdc.compare(six, six) === Option(0))
  }
} // end of ConstantDomainCoreSuite
