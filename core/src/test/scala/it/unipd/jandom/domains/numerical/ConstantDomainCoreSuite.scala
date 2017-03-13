package it.unipd.jandom.domains.numerical

import it.unipd.jandom.domains.ConstantDomainCore.{ConstantBottom, ConstantTop, _}
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

  val positive = Const(6)
  val zero = Const(0)

  "ConstantDomain.inverse" should
    " - return the inverse of the constant value given as input" in {

    val inversePositive = Const(-6)

    assert(inverse(ConstantTop) === ConstantTop)
    assert(inverse(ConstantBottom) === ConstantBottom)
    assert(inverse(positive) === inversePositive)
    // corner case
    assert(inverse(zero) === zero)
  }

  "ConstantDomain.lub" should
    " - return the lub of the constant values given as input" in {
    // ConstantTop
    assert(lub(ConstantTop, positive) === ConstantTop)
    assert(lub(positive, ConstantTop) === ConstantTop)
    assert(lub(ConstantTop, ConstantBottom) === ConstantTop)
    assert(lub(ConstantBottom, ConstantTop) === ConstantTop)
    assert(lub(ConstantTop, ConstantTop) === ConstantTop)
    // ConstantBottom
    assert(lub(ConstantBottom, positive) === positive)
    assert(lub(positive, ConstantBottom) === positive)
    assert(lub(ConstantBottom, ConstantBottom) === ConstantBottom)
    // Const
    assert(lub(positive, zero) === ConstantTop)
    assert(lub(zero, zero) === zero)
  }

  "ConstantDomain.glb" should
    " - return the glb of the constant values given as input" in {

    // ConstantBottom
    assert(glb(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(glb(ConstantBottom, positive) === ConstantBottom)
    assert(glb(ConstantTop, ConstantBottom) === ConstantBottom)
    assert(glb(positive, ConstantBottom) === ConstantBottom)
    assert(glb(ConstantBottom, ConstantBottom) === ConstantBottom)
    // ConstantTop
    assert(glb(ConstantTop, positive) === positive)
    assert(glb(positive, ConstantTop) === positive)
    assert(glb(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(glb(positive, zero) === ConstantBottom)
    assert(glb(zero, zero) === zero)
  }

  "ConstantDomain.sum" should
    " - return the sum of the constant values given as input" in {

    // ConstantBottom
    assert(sum(ConstantBottom, positive) === ConstantBottom)
    assert(sum(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(sum(positive, ConstantBottom) === ConstantBottom)
    assert(sum(ConstantTop, ConstantBottom) === ConstantBottom)
    assert(sum(ConstantBottom, ConstantBottom) === ConstantBottom)
    // ConstantTop
    assert(sum(ConstantTop, positive) === ConstantTop)
    assert(sum(positive, ConstantTop) === ConstantTop)
    assert(sum(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(sum(positive, zero) === positive)
  }

  "ConstantDomain.mult" should
    " - return the product of the constant values given as input" in {

    val multPositive=
      positive match {
        case Const(value) => value*value
      }

    // ConstantBottom
    assert(mult(ConstantBottom, positive) === ConstantBottom)
    assert(mult(ConstantBottom, ConstantTop) === ConstantBottom)
    assert(mult(positive, ConstantBottom) === ConstantBottom)
    assert(mult(ConstantTop, ConstantBottom) === ConstantBottom)
    // Const(0)
    assert(mult(ConstantTop, zero) === zero)
    assert(mult(zero, ConstantTop) === zero)
    assert(mult(zero, positive) === zero)
    assert(mult(positive, zero) === zero)
    assert(mult(zero, zero) === zero)
    // ConstantTop
    assert(mult(ConstantTop, positive) === ConstantTop)
    assert(mult(positive, ConstantTop) === ConstantTop)
    assert(mult(ConstantTop, ConstantTop) === ConstantTop)
    // Const
    assert(mult(positive, positive) === multPositive)
  }

}