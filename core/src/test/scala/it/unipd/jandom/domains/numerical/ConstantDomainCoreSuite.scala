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

}