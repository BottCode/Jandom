package it.unipd.jandom.domains.numerical

import it.unipd.jandom.domains.numerical.sign.SignDomainCore._
import it.unipd.jandom.domains.numerical.sign.Sign._
import org.scalatest.FlatSpec

/**
  * Unit Test - Sign Domain Core
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class SignDomainCoreSuite extends FlatSpec {

  val plus = alpha(6)
  val zero = alpha(0)
  val neg = alpha(-6)

  "SignDomain.inverse" should
    " - return the inverse of the sign value given as input" in {
    assert(inverse(top) === top)
    assert(inverse(bottom) === bottom)
    assert(inverse(plus) === neg)
    assert(inverse(neg) === plus)
    // corner case
    assert(inverse(zero) === zero)
  }

  "SignDomain.lub" should
    " - return the lub of the sign values given as input" in {
    // top
    assert(lub(top, plus) === top)
    assert(lub(plus, top) === top)
    assert(lub(top, neg) === top)
    assert(lub(neg, top) === top)
    assert(lub(top, zero) === top)
    assert(lub(zero, top) === top)
    assert(lub(top, bottom) === top)
    assert(lub(bottom, top) === top)
    assert(lub(top, top) === top)
    // bottom
    assert(lub(bottom, plus) === plus)
    assert(lub(plus, bottom) === plus)
    assert(lub(bottom, neg) === neg)
    assert(lub(neg, bottom) === neg)
    assert(lub(bottom, zero) === zero)
    assert(lub(zero, bottom) === zero)
    assert(lub(bottom, bottom) === bottom)
    // Different signs
    assert(lub(plus, zero) === top)
    assert(lub(zero, plus) === top)
    assert(lub(neg, zero) === top)
    assert(lub(zero, neg) === top)
    assert(lub(neg, plus) === top)
    assert(lub(plus, neg) === top)
    // Equal signs
    assert(lub(zero, zero) === zero)
    assert(lub(plus, plus) === plus)
    assert(lub(neg, neg) === neg)
  }

  "SignDomain.glb" should
    " - return the glb of the sign values given as input" in {
    // top
    assert(glb(top, plus) === plus)
    assert(glb(plus, top) === plus)
    assert(glb(top, neg) === neg)
    assert(glb(neg, top) === neg)
    assert(glb(top, zero) === neg)
    assert(glb(zero, top) === neg)
    assert(glb(top, bottom) === bottom)
    assert(glb(bottom, top) === bottom)
    assert(glb(top, top) === top)
    // bottom
    assert(glb(bottom, plus) === bottom)
    assert(glb(plus, bottom) === bottom)
    assert(glb(bottom, neg) === bottom)
    assert(glb(neg, bottom) === bottom)
    assert(glb(bottom, zero) === bottom)
    assert(glb(zero, bottom) === bottom)
    assert(glb(bottom, bottom) === bottom)
    // Different signs
    assert(glb(plus, zero) === bottom)
    assert(glb(zero, plus) === bottom)
    assert(glb(neg, zero) === bottom)
    assert(glb(zero, neg) === bottom)
    assert(glb(neg, plus) === bottom)
    assert(glb(plus, neg) === bottom)
    // Equal signs
    assert(glb(zero, zero) === zero)
    assert(glb(plus, plus) === plus)
    assert(glb(neg, neg) === neg)
  }

  "SignDomain.sum" should
    " - return the sum of the sign values given as input" in {
    // top
    assert(sum(top, plus) === top)
    assert(sum(plus, top) === top)
    assert(sum(top, neg) === top)
    assert(sum(neg, top) === top)
    assert(sum(top, zero) === top)
    assert(sum(zero, top) === top)
    assert(sum(top, bottom) === bottom)
    assert(sum(bottom, top) === bottom)
    assert(sum(top, top) === top)
    // bottom
    assert(sum(bottom, plus) === bottom)
    assert(sum(plus, bottom) === bottom)
    assert(sum(bottom, neg) === bottom)
    assert(sum(neg, bottom) === bottom)
    assert(sum(bottom, zero) === bottom)
    assert(sum(zero, bottom) === bottom)
    assert(sum(bottom, bottom) === bottom)
    // Different signs
    assert(sum(plus, zero) === plus)
    assert(sum(zero, plus) === plus)
    assert(sum(neg, zero) === neg)
    assert(sum(zero, neg) === neg)
    assert(sum(neg, plus) === top)
    assert(sum(plus, neg) === top)
    // Equal signs
    assert(sum(zero, zero) === zero)
    assert(sum(plus, plus) === plus)
    assert(sum(neg, neg) === neg)
  }

  "SignDomain.mult" should
    " - return the product of the sign values given as input" in {
    // top
    assert(mult(top, plus) === top)
    assert(mult(plus, top) === top)
    assert(mult(top, neg) === top)
    assert(mult(neg, top) === top)
    assert(mult(top, zero) === zero)
    assert(mult(zero, top) === zero)
    assert(mult(top, bottom) === bottom)
    assert(mult(bottom, top) === bottom)
    assert(mult(top, top) === top)
    // bottom
    assert(mult(bottom, plus) === bottom)
    assert(mult(plus, bottom) === bottom)
    assert(mult(bottom, neg) === bottom)
    assert(mult(neg, bottom) === bottom)
    assert(mult(bottom, zero) === bottom)
    assert(mult(zero, bottom) === bottom)
    assert(mult(bottom, bottom) === bottom)
    // Different signs
    assert(mult(plus, zero) === zero)
    assert(mult(zero, plus) === zero)
    assert(mult(neg, zero) === zero)
    assert(mult(zero, neg) === zero)
    assert(mult(neg, plus) === neg)
    assert(mult(plus, neg) === neg)
    // Equal signs
    assert(mult(zero, zero) === zero)
    assert(mult(plus, plus) === plus)
    assert(mult(neg, neg) === plus)
  }

  "SignDomain.division" should
    " - return the division of the sign values given as input" in {
    // top
    assert(division(top, plus) === top)
    assert(division(plus, top) === top)
    assert(division(top, neg) === top)
    assert(division(neg, top) === top)
    assert(division(top, zero) === bottom)
    assert(division(zero, top) === zero)
    assert(division(top, bottom) === bottom)
    assert(division(bottom, top) === bottom)
    assert(division(top, top) === top)
    // bottom
    assert(division(bottom, plus) === bottom)
    assert(division(plus, bottom) === bottom)
    assert(division(bottom, neg) === bottom)
    assert(division(neg, bottom) === bottom)
    assert(division(bottom, zero) === bottom)
    assert(division(zero, bottom) === bottom)
    assert(division(bottom, bottom) === bottom)
    // Different signs
    assert(division(plus, zero) === bottom)
    assert(division(zero, plus) === zero)
    assert(division(neg, zero) === bottom)
    assert(division(zero, neg) === zero)
    assert(division(neg, plus) === top)
    assert(division(plus, neg) === top)
    // Equal signs
    assert(division(zero, zero) === bottom)
    assert(division(plus, plus) === top)
    assert(division(neg, neg) === top)
  }

  "SignDomain.remainder" should
    " - return the remainder (modulo) of the sign values given as input" in {
      // top
      assert(remainder(top, plus) === top)
      assert(remainder(plus, top) === top)
      assert(remainder(top, neg) === top)
      assert(remainder(neg, top) === top)
      assert(remainder(top, zero) === bottom)
      assert(remainder(zero, top) === zero)
      assert(remainder(top, bottom) === bottom)
      assert(remainder(bottom, top) === bottom)
      assert(remainder(top, top) === top)
      // bottom
      assert(remainder(bottom, plus) === bottom)
      assert(remainder(plus, bottom) === bottom)
      assert(remainder(bottom, neg) === bottom)
      assert(remainder(neg, bottom) === bottom)
      assert(remainder(bottom, zero) === bottom)
      assert(remainder(zero, bottom) === bottom)
      assert(remainder(bottom, bottom) === bottom)
      // Different signs
      assert(remainder(plus, zero) === bottom)
      assert(remainder(zero, plus) === zero)
      assert(remainder(neg, zero) === bottom)
      assert(remainder(zero, neg) === zero)
      assert(remainder(neg, plus) === top)
      assert(remainder(plus, neg) === top)
      // Equal signs
      assert(remainder(zero, zero) === bottom)
      assert(remainder(plus, plus) === top)
      assert(remainder(neg, neg) === top)
    }

  "SignDomain.compare" should
    " - return the comparison between the sign values given as input" in {
    // top
    assert(compare(top, plus) === Option(1))
    assert(compare(plus, top) === Option(-1))
    assert(compare(top, neg) === Option(1))
    assert(compare(neg, top) === Option(-1))
    assert(compare(top, zero) === Option(1))
    assert(compare(zero, top) === Option(-1))
    assert(compare(top, bottom) === Option(1))
    assert(compare(bottom, top) === Option(-1))
    assert(compare(top, top) === Option(0))
    // bottom
    assert(compare(bottom, plus) === Option(-1))
    assert(compare(plus, bottom) === Option(1))
    assert(compare(bottom, neg) === Option(-1))
    assert(compare(neg, bottom) === Option(1))
    assert(compare(bottom, zero) === Option(-1))
    assert(compare(zero, bottom) === Option(1))
    assert(compare(bottom, bottom) === Option(0))
    // Different signs
    assert(compare(plus, zero) === Option.empty)
    assert(compare(zero, plus) === Option.empty)
    assert(compare(neg, zero) === Option.empty)
    assert(compare(zero, neg) === Option.empty)
    assert(compare(neg, plus) === Option.empty)
    assert(compare(plus, neg) === Option.empty)
    // Equal signs
    assert(compare(zero, zero) === Option(0))
    assert(compare(plus, plus) === Option(0))
    assert(compare(neg, neg) === Option(0))
  }
} // end of SignDomainCoreSuite
