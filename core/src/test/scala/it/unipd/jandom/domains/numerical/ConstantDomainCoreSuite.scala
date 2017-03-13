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
}