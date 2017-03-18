package it.unipd.jandom.domains.numerical

import it.unipd.jandom.domains.numerical.sign.SignDomainCore._
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

  "SignDomainCore.inverse" should
    " - return the inverse of the sign value given as input" in {
    assert(inverse(plus) === neg)
    assert(inverse(neg) === plus)
    // corner cases
    for (s <- List(zero, top, bottom))
      assert(inverse(s) === s)
  }

  "SignDomainCore.lub" should
    " - return the lub of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, bottom))
      assert(lub(top, s) === top)
    // bottom
    for (s <- List(zero, plus, neg, top))
      assert(lub(bottom, s) === s)
    // Different signs
    assert(lub(plus, zero) === top)
    assert(lub(zero, plus) === top)
    assert(lub(neg, zero) === top)
    assert(lub(zero, neg) === top)
    assert(lub(neg, plus) === top)
    assert(lub(plus, neg) === top)
    // Equal signs
    for (s <- List(zero, plus, neg, top, bottom))
      assert(lub(s, s) === s)
  }

  "SignDomainCore.glb" should
    " - return the glb of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, bottom))
      assert(glb(top, s) === s)
    // bottom
    for (s <- List(zero, plus, neg, top))
      assert(glb(bottom, s) === bottom)
    // Different signs
    assert(glb(plus, zero) === bottom)
    assert(glb(zero, plus) === bottom)
    assert(glb(neg, zero) === bottom)
    assert(glb(zero, neg) === bottom)
    assert(glb(neg, plus) === bottom)
    assert(glb(plus, neg) === bottom)
    // Equal signs
    for (s <- List(zero, plus, neg, top, bottom))
      assert(glb(s, s) === s)
  }

  "SignDomainCore.sum" should
    " - return the sum of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, bottom))
      assert(sum(top, s) === top)
    // bottom
    for (s <- List(zero, plus, neg, top))
      assert(sum(bottom, s) === bottom)
    // Different signs
    assert(sum(plus, zero) === plus)
    assert(sum(zero, plus) === plus)
    assert(sum(neg, zero) === neg)
    assert(sum(zero, neg) === neg)
    assert(sum(neg, plus) === top)
    assert(sum(plus, neg) === top)
    // Equal signs
    for (s <- List(zero, plus, neg, top, bottom))
      assert(sum(s, s) === s)
  }

  "SignDomainCore.mult" should
    " - return the product of the sign values given as input" in {
    // top
    for (s <- List(plus, neg, top))
      assert(mult(top, s) === top)
    for (s <- List(plus, neg))
      assert(mult(s, top) === top)
    // bottom
    for (s <- List(zero, plus, neg, top, bottom))
      assert(mult(bottom, s) === bottom)
    for (s <- List(zero, plus, neg, top))
      assert(mult(s, bottom) === bottom)
    // Zero
    for (s <- List(plus, neg, top, bottom))
      assert(mult(zero, s) === zero)
    for (s <- List(plus, neg, top, bottom))
      assert(mult(s, zero) === zero)
    // Different signs
    assert(mult(neg, plus) === neg)
    assert(mult(plus, neg) === neg)
    // Equal signs
    assert(mult(plus, plus) === plus)
    assert(mult(neg, neg) === plus)
  }

  "SignDomainCore.division" should
    " - return the division of the sign values given as input" in {
    // Division by zero
    for (s <- List(plus, neg, top, bottom, zero))
      assert(division(s, zero) === bottom)
    // Zero over something
    for (s <- List(plus, neg, top))
      assert(division(zero, s) === zero)
    // top
    for (s <- List(plus, neg, top))
      assert(division(s, top) === top)
    for (s <- List(plus, neg))
      assert(division(top, s) === top)
    // bottom
    for (s <- List(plus, neg, top, bottom, zero))
      assert(division(s, bottom) === bottom)
    for (s <- List(plus, neg, top, bottom))
      assert(division(bottom, s) === bottom)
    // Different signs
    assert(division(zero, plus) === zero)
    assert(division(zero, neg) === zero)
    assert(division(neg, plus) === top)
    assert(division(plus, neg) === top)
    // Equal signs
    assert(division(plus, plus) === top)
    assert(division(neg, neg) === top)
  }

  "SignDomainCore.remainder" should
    " - return the remainder (modulo) of the sign values given as input" in {
    // Remainder by zero
    for (s <- List(plus, neg, top, bottom, zero))
      assert(remainder(s, zero) === bottom)
    // Zero modulo something
    for (s <- List(plus, neg, top))
      assert(remainder(zero, s) === zero)
    // top
    for (s <- List(plus, neg, top))
      assert(remainder(s, top) === top)
    for (s <- List(plus, neg))
      assert(remainder(top, s) === top)
      // bottom
    for (s <- List(plus, neg, top, bottom, zero))
      assert(remainder(s, bottom) === bottom)
    for (s <- List(plus, neg, top, bottom))
      assert(remainder(bottom, s) === bottom)
    // Different signs
    assert(remainder(neg, plus) === top)
    assert(remainder(plus, neg) === top)
    // Equal signs
    assert(remainder(plus, plus) === top)
    assert(remainder(neg, neg) === top)
  }

  "SignDomainCore.compare" should
    " - return the comparison between the sign values given as input" in {
    // top
    for (s <- List(plus, neg, bottom, zero))
      assert(compare(top, s) === Option(1))
    for (s <- List(plus, neg, bottom, zero))
      assert(compare(s, top) === Option(-1))
    // bottom
    for (s <- List(plus, neg, top, zero))
      assert(compare(bottom, s) === Option(-1))
    for (s <- List(plus, neg, top, zero))
      assert(compare(s, bottom) === Option(1))
    // Different signs
    assert(compare(plus, zero) === Option.empty)
    assert(compare(zero, plus) === Option.empty)
    assert(compare(neg, zero) === Option.empty)
    assert(compare(zero, neg) === Option.empty)
    assert(compare(neg, plus) === Option.empty)
    assert(compare(plus, neg) === Option.empty)
    // Equal signs
    for (s <- List(plus, neg, bottom, zero, top))
      assert(compare(s, s) === Option(0))
  }
} // end of SignDomainCoreSuite
