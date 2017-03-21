package it.unipd.jandom.domains.numerical.sign

import it.unipd.jandom.domains.numerical.sign.ESeq._
import it.unipd.jandom.domains.numerical.sign.Sign._
import org.scalatest.FlatSpec

/**
  * Unit Test - Sign Domain extended with leq, geq, neq Core
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ESeqDomainCoreSuite extends FlatSpec {

  val dc = ESeqDomainCore()
  val plus = dc.alpha(6)
  val zero = dc.alpha(0)
  val neg = dc.alpha(-6)

  "ESeqDomainCore.inverse" should
    " - return the inverse of the sign value given as input" in {
    assert(dc.inverse(dc.top) === dc.top)
    assert(dc.inverse(dc.bottom) === dc.bottom)
    assert(dc.inverse(plus) === neg)
    assert(dc.inverse(neg) === plus)
    assert(dc.inverse(Geq0) === Leq0)
    assert(dc.inverse(Leq0) === Geq0)
    // corner cases
    for (s <- List(zero, Neq0))
      assert(dc.inverse(s) === s)
  }

  "ESeqDomainCore.lub" should
    " - return the lub of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.bottom))
      assert(dc.lub(dc.top, s) === dc.top)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.bottom))
      assert(dc.lub(s, dc.top) === dc.top)
    // bottom
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.lub(dc.bottom, s) === s)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.lub(s, dc.bottom) === s)
    // Different signs: plus
    assert(dc.lub(plus, zero) === Geq0)
    assert(dc.lub(zero, plus) === Geq0)
    assert(dc.lub(plus, neg) === Neq0)
    assert(dc.lub(neg, plus) === Neq0)
    assert(dc.lub(plus, Geq0) === Geq0)
    assert(dc.lub(Geq0, plus) === Geq0)
    assert(dc.lub(plus, Leq0) === dc.top)
    assert(dc.lub(Leq0, plus) === dc.top)
    assert(dc.lub(plus, Neq0) === Neq0)
    assert(dc.lub(Neq0, plus) === Neq0)
    // Different signs: zero
    assert(dc.lub(zero, neg) === Leq0)
    assert(dc.lub(neg, zero) === Leq0)
    assert(dc.lub(zero, Geq0) === Geq0)
    assert(dc.lub(Geq0, zero) === Geq0)
    assert(dc.lub(zero, Leq0) === Leq0)
    assert(dc.lub(Leq0, zero) === Leq0)
    assert(dc.lub(zero, Neq0) === dc.top)
    assert(dc.lub(Neq0, zero) === dc.top)
    // Different signs: neg
    assert(dc.lub(neg, Geq0) === dc.top)
    assert(dc.lub(Geq0, neg) === dc.top)
    assert(dc.lub(neg, Leq0) === Leq0)
    assert(dc.lub(Leq0, neg) === Leq0)
    assert(dc.lub(neg, Neq0) === Neq0)
    assert(dc.lub(Neq0, neg) === Neq0)
    // Different signs: Geq0
    assert(dc.lub(Geq0, Leq0) === dc.top)
    assert(dc.lub(Leq0, Geq0) === dc.top)
    assert(dc.lub(Geq0, Neq0) === dc.top)
    assert(dc.lub(Neq0, Geq0) === dc.top)
    // Different signs: Leq0
    assert(dc.lub(Leq0, Neq0) === dc.top)
    assert(dc.lub(Neq0, Leq0) === dc.top)
    // Equal signs
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.bottom, dc.top))
      assert(dc.lub(s, s) === s)
  }

  "ESeqDomainCore.glb" should
    " - return the glb of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(dc.glb(dc.top, s) === s)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(dc.glb(s, dc.top) === s)
    // bottom
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.glb(dc.bottom, s) === dc.bottom)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.glb(s, dc.bottom) === dc.bottom)
    // Different signs: plus
    assert(dc.glb(plus, zero) === dc.bottom)
    assert(dc.glb(zero, plus) === dc.bottom)
    assert(dc.glb(plus, neg) === dc.bottom)
    assert(dc.glb(neg, plus) === dc.bottom)
    assert(dc.glb(plus, Geq0) === Plus)
    assert(dc.glb(Geq0, plus) === Plus)
    assert(dc.glb(plus, Leq0) === dc.bottom)
    assert(dc.glb(Leq0, plus) === dc.bottom)
    assert(dc.glb(plus, Neq0) === Plus)
    assert(dc.glb(Neq0, plus) === Plus)
    // Different signs: zero
    assert(dc.glb(zero, neg) === dc.bottom)
    assert(dc.glb(neg, zero) === dc.bottom)
    assert(dc.glb(zero, Geq0) === Zero)
    assert(dc.glb(Geq0, zero) === Zero)
    assert(dc.glb(zero, Leq0) === Zero)
    assert(dc.glb(Leq0, zero) === Zero)
    assert(dc.glb(zero, Neq0) === dc.bottom)
    assert(dc.glb(Neq0, zero) === dc.bottom)
    // Different signs: neg
    assert(dc.glb(neg, Geq0) === dc.bottom)
    assert(dc.glb(Geq0, neg) === dc.bottom)
    assert(dc.glb(neg, Leq0) === Minus)
    assert(dc.glb(Leq0, neg) === Minus)
    assert(dc.glb(neg, Neq0) === Minus)
    assert(dc.glb(Neq0, neg) === Minus)
    // Different signs: Geq0
    assert(dc.glb(Geq0, Leq0) === Zero)
    assert(dc.glb(Leq0, Geq0) === Zero)
    assert(dc.glb(Geq0, Neq0) === Plus)
    assert(dc.glb(Neq0, Geq0) === Plus)
    // Different signs: Leq0
    assert(dc.glb(Leq0, Neq0) === Minus)
    assert(dc.glb(Neq0, Leq0) === Minus)
    // Equal signs
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.bottom, dc.top))
      assert(dc.glb(s, s) === s)
  }

  "ESeqDomainCore.sum" should
    " - return the sum of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(dc.sum(dc.top, s) === dc.top)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(dc.sum(s, dc.top) === dc.top)
    // bottom
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.sum(dc.bottom, s) === dc.bottom)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.sum(s, dc.bottom) === dc.bottom)
    // Different signs: plus
    assert(dc.sum(plus, neg) === dc.top)
    assert(dc.sum(neg, plus) === dc.top)
    assert(dc.sum(plus, Geq0) === Plus)
    assert(dc.sum(Geq0, plus) === Plus)
    assert(dc.sum(plus, Leq0) === dc.top)
    assert(dc.sum(Leq0, plus) === dc.top)
    assert(dc.sum(plus, Neq0) === dc.top)
    assert(dc.sum(Neq0, plus) === dc.top)
    // Different signs: zero
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(dc.sum(zero, s) === s)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(dc.sum(s, zero) === s)
    // Different signs: neg
    assert(dc.sum(neg, Geq0) === dc.top)
    assert(dc.sum(Geq0, neg) === dc.top)
    assert(dc.sum(neg, Leq0) === Minus)
    assert(dc.sum(Leq0, neg) === Minus)
    assert(dc.sum(neg, Neq0) === dc.top)
    assert(dc.sum(Neq0, neg) === dc.top)
    // Different signs: Geq0
    assert(dc.sum(Geq0, Leq0) === dc.top)
    assert(dc.sum(Leq0, Geq0) === dc.top)
    assert(dc.sum(Geq0, Neq0) === dc.top)
    assert(dc.sum(Neq0, Geq0) === dc.top)
    // Different signs: Leq0
    assert(dc.sum(Leq0, Neq0) === dc.top)
    assert(dc.sum(Neq0, Leq0) === dc.top)
    // Equal signs
    for (s <- List(zero, plus, neg, Geq0, Leq0, dc.bottom, dc.top))
      assert(dc.sum(s, s) === s)
    assert(dc.sum(Neq0, Neq0) == dc.top)
  }

  "ESeqDomainCore.mult" should
    " - return the product of the sign values given as input" in {
    // top
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(dc.mult(dc.top, s) === dc.top)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(dc.mult(s, dc.top) === dc.top)
    // bottom
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.mult(dc.bottom, s) === dc.bottom)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.mult(s, dc.bottom) === dc.bottom)
    // Different signs: plus
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(dc.mult(plus, s) === s)
    for (s <- List(neg, Geq0, Leq0, Neq0))
      assert(dc.mult(s, plus) === s)
    // Different signs: zero
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.mult(zero, s) === zero)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(dc.mult(s, zero) === zero)
    // Different signs: neg
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(dc.mult(neg, s) === dc.inverse(s))
    for (s <- List(neg, Geq0, Leq0, Neq0))
      assert(dc.mult(s, neg) === dc.inverse(s))
    // Different signs: Geq0
    assert(dc.mult(Geq0, Leq0) === Leq0)
    assert(dc.mult(Leq0, Geq0) === Leq0)
    assert(dc.mult(Geq0, Neq0) === dc.top)
    assert(dc.mult(Neq0, Geq0) === dc.top)
    // Different signs: Leq0
    assert(dc.mult(Leq0, Neq0) === dc.top)
    assert(dc.mult(Neq0, Leq0) === dc.top)
    // Equal signs
    for (s <- List(Geq0, Neq0, dc.bottom, dc.top))
      assert(dc.mult(s, s) === s)
    assert(dc.mult(Leq0, Leq0) === Geq0)
  }

  "ESeqDomainCore.division" should
    " - return the division of the sign values given as input" in {
    // top
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.division(dc.top, s) === dc.top)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(dc.division(s, dc.top) === dc.top)
    // bottom
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, dc.top, dc.bottom))
      assert(dc.division(dc.bottom, s) === dc.bottom)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.division(s, dc.bottom) === dc.bottom)
    // Different signs: plus
    assert(dc.division(plus, neg) === Leq0)
    assert(dc.division(neg, plus) === Leq0)
    assert(dc.division(plus, Geq0) === Geq0)
    assert(dc.division(Geq0, plus) === Geq0)
    assert(dc.division(plus, Leq0) === Leq0)
    assert(dc.division(Leq0, plus) === Leq0)
    assert(dc.division(plus, Neq0) === dc.top)
    assert(dc.division(Neq0, plus) === dc.top)
    // Different signs: zero
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.division(zero, s) === zero)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(dc.division(s, zero) === dc.bottom)
    // Different signs: neg
    assert(dc.division(neg, Geq0) === Leq0)
    assert(dc.division(Geq0, neg) === Leq0)
    assert(dc.division(neg, Leq0) === Geq0)
    assert(dc.division(Leq0, neg) === Geq0)
    assert(dc.division(neg, Neq0) === dc.top)
    assert(dc.division(Neq0, neg) === dc.top)
    // Different signs: Geq0
    assert(dc.division(Geq0, Leq0) === Leq0)
    assert(dc.division(Leq0, Geq0) === Leq0)
    assert(dc.division(Geq0, Neq0) === dc.top)
    assert(dc.division(Neq0, Geq0) === dc.top)
    // Different signs: Leq0
    assert(dc.division(Leq0, Neq0) === dc.top)
    assert(dc.division(Neq0, Leq0) === dc.top)
    // Equal signs
    for (s <- List(plus, neg, Geq0, Leq0))
      assert(dc.division(s, s) === Geq0)
    assert(dc.division(Neq0, Neq0) === dc.top)
  }

  "ESeqDomainCore.remainder" should
    " - return the remainder (modulo) of the sign values given as input" in {
    // top
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.remainder(dc.top, s) === dc.top)
    assert(dc.remainder(plus, dc.top) === Geq0)
    assert(dc.remainder(Geq0, dc.top) === Geq0)
    assert(dc.remainder(neg, dc.top) === Leq0)
    assert(dc.remainder(Leq0, dc.top) === Leq0)
    assert(dc.remainder(Neq0, dc.top) === dc.top)
    // bottom
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, dc.top, dc.bottom))
      assert(dc.remainder(dc.bottom, s) === dc.bottom)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.remainder(s, dc.bottom) === dc.bottom)
    // Different signs: plus
    assert(dc.remainder(plus, neg) === Geq0)
    assert(dc.remainder(neg, plus) === Leq0)
    assert(dc.remainder(plus, Geq0) === Geq0)
    assert(dc.remainder(Geq0, plus) === Geq0)
    assert(dc.remainder(plus, Leq0) === Geq0)
    assert(dc.remainder(Leq0, plus) === Leq0)
    assert(dc.remainder(plus, Neq0) === Geq0)
    assert(dc.remainder(Neq0, plus) === dc.top)
    // Different signs: zero
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, dc.top))
      assert(dc.remainder(zero, s) === zero)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(dc.remainder(s, zero) === dc.bottom)
    // Different signs: neg
    assert(dc.remainder(neg, Geq0) === Leq0)
    assert(dc.remainder(Geq0, neg) === Geq0)
    assert(dc.remainder(neg, Leq0) === Leq0)
    assert(dc.remainder(Leq0, neg) === Leq0)
    assert(dc.remainder(neg, Neq0) === Leq0)
    assert(dc.remainder(Neq0, neg) === dc.top)
    // Different signs: Geq0
    assert(dc.remainder(Geq0, Leq0) === Geq0)
    assert(dc.remainder(Leq0, Geq0) === Leq0)
    assert(dc.remainder(Geq0, Neq0) === Geq0)
    assert(dc.remainder(Neq0, Geq0) === dc.top)
    // Different signs: Leq0
    assert(dc.remainder(Leq0, Neq0) === Leq0)
    assert(dc.remainder(Neq0, Leq0) === dc.top)
    // Equal signs
    assert(dc.remainder(plus, plus) === Geq0)
    assert(dc.remainder(neg, neg) === Leq0)
    assert(dc.remainder(Geq0, Geq0) === Geq0)
    assert(dc.remainder(Leq0, Leq0) === Leq0)
    assert(dc.remainder(Neq0, Neq0) === dc.top)
  }

  "ESeqDomainCore.compare" should
    " - return the comparison between the sign values given as input" in {
    // top
    for (s <- List(plus, neg, dc.bottom, zero, Leq0, Geq0, Neq0))
      assert(dc.compare(dc.top, s) === Option(1))
    for (s <- List(plus, neg, dc.bottom, zero, Leq0, Geq0, Neq0))
      assert(dc.compare(s, dc.top) === Option(-1))
    // bottom
    for (s <- List(plus, neg, dc.top, zero, Leq0, Geq0, Neq0))
      assert(dc.compare(dc.bottom, s) === Option(-1))
    for (s <- List(plus, neg, dc.top, zero, Leq0, Geq0, Neq0))
      assert(dc.compare(s, dc.bottom) === Option(1))
    // Different signs: plus
    assert(dc.compare(plus, zero) === Option.empty)
    assert(dc.compare(zero, plus) === Option.empty)
    assert(dc.compare(plus, neg) === Option.empty)
    assert(dc.compare(neg, plus) === Option.empty)
    assert(dc.compare(plus, Geq0) === Option(-1))
    assert(dc.compare(Geq0, plus) === Option(1))
    assert(dc.compare(plus, Leq0) === Option.empty)
    assert(dc.compare(Leq0, plus) === Option.empty)
    assert(dc.compare(plus, Neq0) === Option(-1))
    assert(dc.compare(Neq0, plus) === Option(1))
    // Different signs: zero
    assert(dc.compare(zero, neg) === Option.empty)
    assert(dc.compare(neg, zero) === Option.empty)
    assert(dc.compare(zero, Geq0) === Option(-1))
    assert(dc.compare(Geq0, zero) === Option(1))
    assert(dc.compare(zero, Leq0) === Option(-1))
    assert(dc.compare(Leq0, zero) === Option(1))
    assert(dc.compare(zero, Neq0) === Option.empty)
    assert(dc.compare(Neq0, zero) === Option.empty)
    // Different signs: neg
    assert(dc.compare(neg, Geq0) === Option.empty)
    assert(dc.compare(Geq0, neg) === Option.empty)
    assert(dc.compare(neg, Leq0) === Option(-1))
    assert(dc.compare(Leq0, neg) === Option(1))
    assert(dc.compare(neg, Neq0) === Option(-1))
    assert(dc.compare(Neq0, neg) === Option(1))
    // Different signs: Geq0
    assert(dc.compare(Geq0, Leq0) === Option.empty)
    assert(dc.compare(Leq0, Geq0) === Option.empty)
    assert(dc.compare(Geq0, Neq0) === Option.empty)
    assert(dc.compare(Neq0, Geq0) === Option.empty)
    // Different signs: Leq0
    assert(dc.compare(Leq0, Neq0) === Option.empty)
    assert(dc.compare(Neq0, Leq0) === Option.empty)
    // Equal signs
    for (s <- List(plus, neg, dc.bottom, zero, dc.top, Leq0, Geq0, Neq0))
      assert(dc.compare(s, s) === Option(0))
  }
} // end of ESeqDomainCoreSuite
