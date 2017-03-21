package it.unipd.jandom.domains.numerical.sign

import org.scalatest.FlatSpec

/**
  * Unit Test - Sign Domain Core
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class SignDomainCoreSuite extends FlatSpec {

  val dc = SignDomainCore()
  val plus = dc.alpha(6)
  val zero = dc.alpha(0)
  val neg = dc.alpha(-6)

  "SignDomainCore.inverse" should
    " - return the inverse of the sign value given as input" in {
    assert(dc.inverse(plus) === neg)
    assert(dc.inverse(neg) === plus)
    // corner cases
    for (s <- List(zero, dc.top, dc.bottom))
      assert(dc.inverse(s) === s)
  }

  "SignDomainCore.lub" should
    " - return the lub of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, dc.bottom))
      assert(dc.lub(dc.top, s) === dc.top)
    for (s <- List(zero, plus, neg, dc.bottom))
      assert(dc.lub(s, dc.top) === dc.top)
    // bottom
    for (s <- List(zero, plus, neg, dc.top))
      assert(dc.lub(dc.bottom, s) === s)
    for (s <- List(zero, plus, neg, dc.top))
      assert(dc.lub(s, dc.bottom) === s)
    // Different signs
    assert(dc.lub(plus, zero) === dc.top)
    assert(dc.lub(zero, plus) === dc.top)
    assert(dc.lub(neg, zero) === dc.top)
    assert(dc.lub(zero, neg) === dc.top)
    assert(dc.lub(neg, plus) === dc.top)
    assert(dc.lub(plus, neg) === dc.top)
    // Equal signs
    for (s <- List(zero, plus, neg, dc.top, dc.bottom))
      assert(dc.lub(s, s) === s)
  }

  "SignDomainCore.glb" should
    " - return the glb of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, dc.bottom))
      assert(dc.glb(dc.top, s) === s)
    for (s <- List(zero, plus, neg, dc.bottom))
      assert(dc.glb(s, dc.top) === s)
    // bottom
    for (s <- List(zero, plus, neg, dc.top))
      assert(dc.glb(dc.bottom, s) === dc.bottom)
    for (s <- List(zero, plus, neg, dc.top))
      assert(dc.glb(s, dc.bottom) === dc.bottom)
    // Different signs
    assert(dc.glb(plus, zero) === dc.bottom)
    assert(dc.glb(zero, plus) === dc.bottom)
    assert(dc.glb(neg, zero) === dc.bottom)
    assert(dc.glb(zero, neg) === dc.bottom)
    assert(dc.glb(neg, plus) === dc.bottom)
    assert(dc.glb(plus, neg) === dc.bottom)
    // Equal signs
    for (s <- List(zero, plus, neg, dc.top, dc.bottom))
      assert(dc.glb(s, s) === s)
  }

  "SignDomainCore.sum" should
    " - return the sum of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg))
      assert(dc.sum(dc.top, s) === dc.top)
    for (s <- List(zero, plus, neg))
      assert(dc.sum(s, dc.top) === dc.top)
    // bottom
    for (s <- List(zero, plus, neg, dc.top))
      assert(dc.sum(dc.bottom, s) === dc.bottom)
    for (s <- List(zero, plus, neg, dc.top))
      assert(dc.sum(s, dc.bottom) === dc.bottom)
    // Different signs
    assert(dc.sum(plus, zero) === plus)
    assert(dc.sum(zero, plus) === plus)
    assert(dc.sum(neg, zero) === neg)
    assert(dc.sum(zero, neg) === neg)
    assert(dc.sum(neg, plus) === dc.top)
    assert(dc.sum(plus, neg) === dc.top)
    // Equal signs
    for (s <- List(zero, plus, neg, dc.top, dc.bottom))
      assert(dc.sum(s, s) === s)
  }

  "SignDomainCore.mult" should
    " - return the product of the sign values given as input" in {
    // top
    for (s <- List(plus, neg, dc.top))
      assert(dc.mult(dc.top, s) === dc.top)
    for (s <- List(plus, neg))
      assert(dc.mult(s, dc.top) === dc.top)
    // bottom
    for (s <- List(zero, plus, neg, dc.top, dc.bottom))
      assert(dc.mult(dc.bottom, s) === dc.bottom)
    for (s <- List(zero, plus, neg, dc.top))
      assert(dc.mult(s, dc.bottom) === dc.bottom)
    // Zero
    for (s <- List(plus, neg, dc.top))
      assert(dc.mult(zero, s) === zero)
    for (s <- List(plus, neg, dc.top))
      assert(dc.mult(s, zero) === zero)
    // Different signs
    assert(dc.mult(neg, plus) === neg)
    assert(dc.mult(plus, neg) === neg)
    // Equal signs
    assert(dc.mult(plus, plus) === plus)
    assert(dc.mult(neg, neg) === plus)
  }

  "SignDomainCore.division" should
    " - return the division of the sign values given as input" in {
    // Division by zero
    for (s <- List(plus, neg, dc.top, dc.bottom, zero))
      assert(dc.division(s, zero) === dc.bottom)
    // Zero over something
    for (s <- List(plus, neg, dc.top))
      assert(dc.division(zero, s) === zero)
    // top
    for (s <- List(plus, neg, dc.top))
      assert(dc.division(s, dc.top) === dc.top)
    for (s <- List(plus, neg))
      assert(dc.division(dc.top, s) === dc.top)
    // bottom
    for (s <- List(plus, neg, dc.top, dc.bottom, zero))
      assert(dc.division(s, dc.bottom) === dc.bottom)
    for (s <- List(plus, neg, dc.top, dc.bottom))
      assert(dc.division(dc.bottom, s) === dc.bottom)
    // Different signs
    assert(dc.division(zero, plus) === zero)
    assert(dc.division(zero, neg) === zero)
    assert(dc.division(neg, plus) === dc.top)
    assert(dc.division(plus, neg) === dc.top)
    // Equal signs
    assert(dc.division(plus, plus) === dc.top)
    assert(dc.division(neg, neg) === dc.top)
  }

  "SignDomainCore.remainder" should
    " - return the remainder (modulo) of the sign values given as input" in {
    // Remainder by zero
    for (s <- List(plus, neg, dc.top, dc.bottom, zero))
      assert(dc.remainder(s, zero) === dc.bottom)
    // Zero modulo something
    for (s <- List(plus, neg, dc.top))
      assert(dc.remainder(zero, s) === zero)
    // top
    for (s <- List(plus, neg, dc.top))
      assert(dc.remainder(s, dc.top) === dc.top)
    for (s <- List(plus, neg))
      assert(dc.remainder(dc.top, s) === dc.top)
      // bottom
    for (s <- List(plus, neg, dc.top, dc.bottom, zero))
      assert(dc.remainder(s, dc.bottom) === dc.bottom)
    for (s <- List(plus, neg, dc.top, dc.bottom))
      assert(dc.remainder(dc.bottom, s) === dc.bottom)
    // Different signs
    assert(dc.remainder(neg, plus) === dc.top)
    assert(dc.remainder(plus, neg) === dc.top)
    // Equal signs
    assert(dc.remainder(plus, plus) === dc.top)
    assert(dc.remainder(neg, neg) === dc.top)
  }

  "SignDomainCore.compare" should
    " - return the comparison between the sign values given as input" in {
    // top
    for (s <- List(plus, neg, dc.bottom, zero))
      assert(dc.compare(dc.top, s) === Option(1))
    for (s <- List(plus, neg, dc.bottom, zero))
      assert(dc.compare(s, dc.top) === Option(-1))
    // bottom
    for (s <- List(plus, neg, dc.top, zero))
      assert(dc.compare(dc.bottom, s) === Option(-1))
    for (s <- List(plus, neg, dc.top, zero))
      assert(dc.compare(s, dc.bottom) === Option(1))
    // Different signs
    assert(dc.compare(plus, zero) === Option.empty)
    assert(dc.compare(zero, plus) === Option.empty)
    assert(dc.compare(neg, zero) === Option.empty)
    assert(dc.compare(zero, neg) === Option.empty)
    assert(dc.compare(neg, plus) === Option.empty)
    assert(dc.compare(plus, neg) === Option.empty)
    // Equal signs
    for (s <- List(plus, neg, dc.bottom, zero, dc.top))
      assert(dc.compare(s, s) === Option(0))
  }
} // end of SignDomainCoreSuite
