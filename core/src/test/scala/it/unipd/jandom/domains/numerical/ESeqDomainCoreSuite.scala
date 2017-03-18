package it.unipd.jandom.domains.numerical

import it.unipd.jandom.domains.numerical.sign.ESeq._
import it.unipd.jandom.domains.numerical.sign.ESeqDomainCore._
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

  val plus = alpha(6)
  val zero = alpha(0)
  val neg = alpha(-6)

  "ESeqDomainCore.inverse" should
    " - return the inverse of the sign value given as input" in {
    assert(inverse(top) === top)
    assert(inverse(bottom) === bottom)
    assert(inverse(plus) === neg)
    assert(inverse(neg) === plus)
    assert(inverse(Geq0) === Leq0)
    assert(inverse(Leq0) === Geq0)
    // corner cases
    for (s <- List(zero, Neq0))
      assert(inverse(s) === s)
  }

  "ESeqDomainCore.lub" should
    " - return the lub of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, bottom))
      assert(lub(top, s) === top)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, bottom))
      assert(lub(s, top) === top)
    // bottom
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, top))
      assert(lub(bottom, s) === s)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, top))
      assert(lub(s, bottom) === s)
    // Different signs: plus
    assert(lub(plus, zero) === Geq0)
    assert(lub(zero, plus) === Geq0)
    assert(lub(plus, neg) === Neq0)
    assert(lub(neg, plus) === Neq0)
    assert(lub(plus, Geq0) === Geq0)
    assert(lub(Geq0, plus) === Geq0)
    assert(lub(plus, Leq0) === top)
    assert(lub(Leq0, plus) === top)
    assert(lub(plus, Neq0) === Neq0)
    assert(lub(Neq0, plus) === Neq0)
    // Different signs: zero
    assert(lub(zero, neg) === Leq0)
    assert(lub(neg, zero) === Leq0)
    assert(lub(zero, Geq0) === Geq0)
    assert(lub(Geq0, zero) === Geq0)
    assert(lub(zero, Leq0) === Leq0)
    assert(lub(Leq0, zero) === Leq0)
    assert(lub(zero, Neq0) === top)
    assert(lub(Neq0, zero) === top)
    // Different signs: neg
    assert(lub(neg, Geq0) === top)
    assert(lub(Geq0, neg) === top)
    assert(lub(neg, Leq0) === Leq0)
    assert(lub(Leq0, neg) === Leq0)
    assert(lub(neg, Neq0) === Neq0)
    assert(lub(Neq0, neg) === Neq0)
    // Different signs: Geq0
    assert(lub(Geq0, Leq0) === top)
    assert(lub(Leq0, Geq0) === top)
    assert(lub(Geq0, Neq0) === top)
    assert(lub(Neq0, Geq0) === top)
    // Different signs: Leq0
    assert(lub(Leq0, Neq0) === top)
    assert(lub(Neq0, Leq0) === top)
    // Equal signs
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, bottom, top))
      assert(lub(s, s) === s)
  }

  "ESeqDomainCore.glb" should
    " - return the glb of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(glb(top, s) === s)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(glb(s, top) === s)
    // bottom
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, top))
      assert(glb(bottom, s) === bottom)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, top))
      assert(glb(s, bottom) === bottom)
    // Different signs: plus
    assert(glb(plus, zero) === bottom)
    assert(glb(zero, plus) === bottom)
    assert(glb(plus, neg) === bottom)
    assert(glb(neg, plus) === bottom)
    assert(glb(plus, Geq0) === Plus)
    assert(glb(Geq0, plus) === Plus)
    assert(glb(plus, Leq0) === bottom)
    assert(glb(Leq0, plus) === bottom)
    assert(glb(plus, Neq0) === Plus)
    assert(glb(Neq0, plus) === Plus)
    // Different signs: zero
    assert(glb(zero, neg) === bottom)
    assert(glb(neg, zero) === bottom)
    assert(glb(zero, Geq0) === Zero)
    assert(glb(Geq0, zero) === Zero)
    assert(glb(zero, Leq0) === Zero)
    assert(glb(Leq0, zero) === Zero)
    assert(glb(zero, Neq0) === bottom)
    assert(glb(Neq0, zero) === bottom)
    // Different signs: neg
    assert(glb(neg, Geq0) === bottom)
    assert(glb(Geq0, neg) === bottom)
    assert(glb(neg, Leq0) === Minus)
    assert(glb(Leq0, neg) === Minus)
    assert(glb(neg, Neq0) === Minus)
    assert(glb(Neq0, neg) === Minus)
    // Different signs: Geq0
    assert(glb(Geq0, Leq0) === Zero)
    assert(glb(Leq0, Geq0) === Zero)
    assert(glb(Geq0, Neq0) === Plus)
    assert(glb(Neq0, Geq0) === Plus)
    // Different signs: Leq0
    assert(glb(Leq0, Neq0) === Minus)
    assert(glb(Neq0, Leq0) === Minus)
    // Equal signs
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, bottom, top))
      assert(glb(s, s) === s)
  }

  "ESeqDomainCore.sum" should
    " - return the sum of the sign values given as input" in {
    // top
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(sum(top, s) === top)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(sum(s, top) === top)
    // bottom
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, top))
      assert(sum(bottom, s) === bottom)
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, top))
      assert(sum(s, bottom) === bottom)
    // Different signs: plus
    assert(sum(plus, neg) === top)
    assert(sum(neg, plus) === top)
    assert(sum(plus, Geq0) === Plus)
    assert(sum(Geq0, plus) === Plus)
    assert(sum(plus, Leq0) === top)
    assert(sum(Leq0, plus) === top)
    assert(sum(plus, Neq0) === top)
    assert(sum(Neq0, plus) === top)
    // Different signs: zero
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0))
      assert(sum(zero, s) === s)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(sum(s, zero) === s)
    // Different signs: neg
    assert(sum(neg, Geq0) === top)
    assert(sum(Geq0, neg) === top)
    assert(sum(neg, Leq0) === Minus)
    assert(sum(Leq0, neg) === Minus)
    assert(sum(neg, Neq0) === top)
    assert(sum(Neq0, neg) === top)
    // Different signs: Geq0
    assert(sum(Geq0, Leq0) === top)
    assert(sum(Leq0, Geq0) === top)
    assert(sum(Geq0, Neq0) === top)
    assert(sum(Neq0, Geq0) === top)
    // Different signs: Leq0
    assert(sum(Leq0, Neq0) === top)
    assert(sum(Neq0, Leq0) === top)
    // Equal signs
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, bottom, top))
      assert(sum(s, s) === s)
  }

  "ESeqDomainCore.mult" should
    " - return the product of the sign values given as input" in {
    // top
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(mult(top, s) === top)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(mult(s, top) === top)
    // bottom
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, top))
      assert(mult(bottom, s) === bottom)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, top))
      assert(mult(s, bottom) === bottom)
    // Different signs: plus
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(mult(plus, s) === s)
    for (s <- List(neg, Geq0, Leq0, Neq0))
      assert(mult(s, plus) === s)
    // Different signs: zero
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, top))
      assert(mult(zero, s) === zero)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(mult(s, zero) === zero)
    // Different signs: neg
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(mult(neg, s) === inverse(s))
    for (s <- List(neg, Geq0, Leq0, Neq0))
      assert(mult(s, neg) === inverse(s))
    // Different signs: Geq0
    assert(mult(Geq0, Leq0) === top)
    assert(mult(Leq0, Geq0) === top)
    assert(mult(Geq0, Neq0) === top)
    assert(mult(Neq0, Geq0) === top)
    // Different signs: Leq0
    assert(mult(Leq0, Neq0) === top)
    assert(mult(Neq0, Leq0) === top)
    // Equal signs
    for (s <- List(Geq0, Neq0, bottom, top))
      assert(mult(s, s) === s)
    assert(mult(Leq0, Leq0) === Geq0)
  }

  "ESeqDomainCore.division" should
    " - return the division of the sign values given as input" in {
    // top
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, top))
      assert(division(top, s) === top)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(division(s, top) === top)
    // bottom
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, top, bottom))
      assert(division(bottom, s) === bottom)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, top))
      assert(division(s, bottom) === bottom)
    // Different signs: plus
    assert(division(plus, neg) === Leq0)
    assert(division(neg, plus) === Leq0)
    assert(division(plus, Geq0) === Geq0)
    assert(division(Geq0, plus) === Geq0)
    assert(division(plus, Leq0) === Leq0)
    assert(division(Leq0, plus) === Leq0)
    assert(division(plus, Neq0) === top)
    assert(division(Neq0, plus) === top)
    // Different signs: zero
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, top))
      assert(division(zero, s) === zero)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(division(s, zero) === bottom)
    // Different signs: neg
    assert(division(neg, Geq0) === Leq0)
    assert(division(Geq0, neg) === Leq0)
    assert(division(neg, Leq0) === Geq0)
    assert(division(Leq0, neg) === Geq0)
    assert(division(neg, Neq0) === top)
    assert(division(Neq0, neg) === top)
    // Different signs: Geq0
    assert(division(Geq0, Leq0) === Leq0)
    assert(division(Leq0, Geq0) === Leq0)
    assert(division(Geq0, Neq0) === top)
    assert(division(Neq0, Geq0) === top)
    // Different signs: Leq0
    assert(division(Leq0, Neq0) === top)
    assert(division(Neq0, Leq0) === top)
    // Equal signs
    for (s <- List(plus, neg, Geq0, Leq0))
      assert(division(s, s) === Geq0)
    assert(division(Neq0, Neq0) === top)
  }

  "ESeqDomainCore.remainder" should
    " - return the remainder (modulo) of the sign values given as input" in {
    // top
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, top))
      assert(remainder(top, s) === top)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(remainder(s, top) === top)
    // bottom
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, top, bottom))
      assert(remainder(bottom, s) === bottom)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0, top))
      assert(remainder(s, bottom) === bottom)
    // Different signs: plus
    assert(remainder(plus, neg) === Geq0)
    assert(remainder(neg, plus) === Leq0)
    assert(remainder(plus, Geq0) === Geq0)
    assert(remainder(Geq0, plus) === Geq0)
    assert(remainder(plus, Leq0) === Geq0)
    assert(remainder(Leq0, plus) === Leq0)
    assert(remainder(plus, Neq0) === Geq0)
    assert(remainder(Neq0, plus) === top)
    // Different signs: zero
    for (s <- List(zero, plus, neg, Geq0, Leq0, Neq0, top))
      assert(remainder(zero, s) === zero)
    for (s <- List(plus, neg, Geq0, Leq0, Neq0))
      assert(remainder(s, zero) === bottom)
    // Different signs: neg
    assert(remainder(neg, Geq0) === Leq0)
    assert(remainder(Geq0, neg) === Geq0)
    assert(remainder(neg, Leq0) === Leq0)
    assert(remainder(Leq0, neg) === Leq0)
    assert(remainder(neg, Neq0) === Leq0)
    assert(remainder(Neq0, neg) === top)
    // Different signs: Geq0
    assert(remainder(Geq0, Leq0) === Geq0)
    assert(remainder(Leq0, Geq0) === Leq0)
    assert(remainder(Geq0, Neq0) === Geq0)
    assert(remainder(Neq0, Geq0) === top)
    // Different signs: Leq0
    assert(remainder(Leq0, Neq0) === Leq0)
    assert(remainder(Neq0, Leq0) === top)
    // Equal signs
    assert(remainder(plus, plus) === Geq0)
    assert(remainder(neg, neg) === Leq0)
    assert(remainder(Geq0, Geq0) === Geq0)
    assert(remainder(Leq0, Leq0) === Leq0)
    assert(remainder(Neq0, Neq0) === top)
  }

  "ESeqDomainCore.compare" should
    " - return the comparison between the sign values given as input" in {
    // top
    for (s <- List(plus, neg, bottom, zero, Leq0, Geq0, Neq0))
      assert(compare(top, s) === Option(1))
    for (s <- List(plus, neg, bottom, zero, Leq0, Geq0, Neq0))
      assert(compare(s, top) === Option(-1))
    // bottom
    for (s <- List(plus, neg, top, zero, Leq0, Geq0, Neq0))
      assert(compare(bottom, s) === Option(-1))
    for (s <- List(plus, neg, top, zero, Leq0, Geq0, Neq0))
      assert(compare(s, bottom) === Option(1))
    // Different signs: plus
    assert(compare(plus, zero) === Option.empty)
    assert(compare(zero, plus) === Option.empty)
    assert(compare(plus, neg) === Option.empty)
    assert(compare(neg, plus) === Option.empty)
    assert(compare(plus, Geq0) === Option(-1))
    assert(compare(Geq0, plus) === Option(1))
    assert(compare(plus, Leq0) === Option.empty)
    assert(compare(Leq0, plus) === Option.empty)
    assert(compare(plus, Neq0) === Option(-1))
    assert(compare(Neq0, plus) === Option(1))
    // Different signs: zero
    assert(compare(zero, neg) === Option.empty)
    assert(compare(neg, zero) === Option.empty)
    assert(compare(zero, Geq0) === Option(-1))
    assert(compare(Geq0, zero) === Option(1))
    assert(compare(zero, Leq0) === Option(-1))
    assert(compare(Leq0, zero) === Option(1))
    assert(compare(zero, Neq0) === Option.empty)
    assert(compare(Neq0, zero) === Option.empty)
    // Different signs: neg
    assert(compare(neg, Geq0) === Option.empty)
    assert(compare(Geq0, neg) === Option.empty)
    assert(compare(neg, Leq0) === Option(-1))
    assert(compare(Leq0, neg) === Option(1))
    assert(compare(neg, Neq0) === Option(-1))
    assert(compare(Neq0, neg) === Option(1))
    // Different signs: Geq0
    assert(compare(Geq0, Leq0) === Option.empty)
    assert(compare(Leq0, Geq0) === Option.empty)
    assert(compare(Geq0, Neq0) === Option.empty)
    assert(compare(Neq0, Geq0) === Option.empty)
    // Different signs: Leq0
    assert(compare(Leq0, Neq0) === Option.empty)
    assert(compare(Neq0, Leq0) === Option.empty)
    // Equal signs
    for (s <- List(plus, neg, bottom, zero, top, Leq0, Geq0, Neq0))
      assert(compare(s, s) === Option(0))
  }
} // end of ESeqDomainCoreSuite
