package it.unipd.jandom.domains.numerical.congruence

import org.scalatest.FlatSpec

/**
  * Unit Test - Congruence Domain Core
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class CongruenceDomainCoreSuite extends FlatSpec {

  val dc = CongruenceDomainCore()
  val twoZnone= Congruence.Mod(Some(2),-1) /*2Z-1*/
  /* standard form */
  val twoZzero= Congruence.Mod(Some(2),0) /*2Z+0*/
  val twoZone= Congruence.Mod(Some(2),1) /*2Z+1*/
  val twoZtwo= Congruence.Mod(Some(2),2) /*2Z+2*/
  val sixZone= Congruence.Mod(Some(6),1) /*6Z+1*/
  val nineZone= Congruence.Mod(Some(9),1) /*9Z+1*/
  val eightZzero= Congruence.Mod(Some(8),0) /*8Z+0*/
  val eightZone= Congruence.Mod(Some(8),1) /*8Z+1*/
  val eightZtwo= Congruence.Mod(Some(8),2) /*8Z+2*/
  val threeZnone= Congruence.Mod(Some(3),-1) /*3Z-1*/
  val threeZone= Congruence.Mod(Some(3),1) /*3Z+1*/
  val threeZtwo= Congruence.Mod(Some(3),2) /*3Z+2*/
  val threeZzero= Congruence.Mod(Some(3),0) /*3Z+0*/
  val three= Congruence.Mod(None,3) /*0Z+3*/
  val nine= Congruence.Mod(None,9) /*0Z+9*/
  val zero= Congruence.Mod(None,0) /*0Z+0*/ 

  "CongruenceDomainCore.alpha" should
    " - return the corresponding abstract value (check reduction aka standardForm)" 
    in {
      /* reduction (2Z+X*n) = 2Z+X with n in N */
    assert(dc.alpha(Some(2),3) === twoZone)
    assert(dc.alpha(Some(6),7) === sixZone)
    assert(dc.alpha(Some(9),19) === nineZone)
    assert(dc.alpha(None,1) !== three)
    assert(dc.alpha(None,3) === three)
  }

  "CongruenceDomainCore.sum" should
    " - return the sum of the two congruences given as input" in {
    /* BOTTOM */
    assert(dc.sum(dc.bottom, dc.bottom) === dc.bottom)
    assert(dc.sum(dc.bottom, three) === dc.bottom)
    assert(dc.sum(nineZone, dc.bottom) === dc.bottom)
    /* TOP */
    assert(dc.sum(dc.top, dc.top) === dc.top)
    assert(dc.sum(dc.top, three) === dc.top)
    assert(dc.sum(nineZone, dc.top) === dc.top)
    /* BOTTOM < X < TOP */
    assert(dc.sum(sixZone, nineZone) === threeZtwo)
    assert(dc.sum(eightZone, twoZone) === twoZzero)
  }

  "CongruenceDomainCore.inverse" should
    " - return the inverse of the congruence given as input" in {
    /* FIXED ELEMENTS (BOTTOM, TOP, ZERO_CONSTANT */
    assert(dc.inverse(dc.bottom) === dc.bottom)
    assert(dc.inverse(dc.top) === dc.top)
    assert(dc.inverse(twoZzero) === twoZzero)
    /* NOT FIXED ELEMENTS */
    assert(dc.inverse(threeZnone) === threeZone)
    assert(dc.inverse(twoZone) === twoZone)
  }

/* 
  Note: we always consider the standardForm in the result. 
        So, each result is in the form aZ+b with a>0 and b >= 0
*/
  "CongruenceDomainCore.mult" should
    " - return the mult of the two congruences given as input" in {
    /* BOTTOM */
    assert(dc.mult(dc.bottom, dc.bottom) === dc.bottom)
    assert(dc.mult(dc.bottom, three) === dc.bottom)
    assert(dc.mult(nineZone, dc.bottom) === dc.bottom)
    /* TOP */
    assert(dc.mult(dc.top, dc.top) === dc.top)
    /*a= gcd(0,0,3) = 3, b = 1* 0= 0*/
    assert(dc.mult(dc.top, three) === threeZzero)
    assert(dc.mult(twoZone, dc.top) === dc.top)
    /* BOTTOM < X < TOP */
    assert(dc.mult(three, three) === nine)
    assert(dc.mult(twoZone, twoZone) === twoZone)
    assert(dc.mult(twoZone, twoZzero) === twoZzero)
  }

/*
  division returns always top if it is not a "corner case" 
  (i.e. bottom, division by zero, division between constants): 
  example -
  2Z+0 = {..-2,0,2,4,6...}
  2Z+2 = {..-2,0,2,4,6...}
  2Z+0/2Z+2= {...-1,0,1,2,3...} = 1Z+0 = TOP
*/
  "CongruenceDomainCore.division" should
    " - return the division of the two congruences given as input" in {
    /* BOTTOM */
    assert(dc.division(dc.bottom, dc.bottom) === dc.bottom)
    assert(dc.division(dc.bottom, three) === dc.bottom)
    assert(dc.division(nineZone, dc.bottom) === dc.bottom)
    /* DIVISION BY ZERO */
    assert(dc.division(twoZone, zero) === dc.bottom)
    /* TOP */
    assert(dc.division(dc.top, dc.top) === dc.top)
    assert(dc.division(dc.top, three) === dc.top)
    assert(dc.division(twoZone, dc.top) === dc.top)
    /* BOTTOM < X < TOP */
    assert(dc.division(nine, three) === three)
    assert(dc.division(twoZone, twoZzero) === dc.top)
    assert(dc.division(twoZzero, twoZtwo) === dc.top)
  }

  "CongruenceDomainCore.remainder" should
    " - return the remainder of the two congruences given as input" in {
    /* BOTTOM */
    assert(dc.remainder(dc.bottom, dc.bottom) === dc.bottom)
    assert(dc.remainder(dc.bottom, three) === dc.bottom)
    assert(dc.remainder(nineZone, dc.bottom) === dc.bottom)
    /* DIVISION BY ZERO */
    assert(dc.remainder(twoZone, zero) === dc.bottom)
    /* TOP */
    assert(dc.remainder(dc.top, dc.top) === dc.top)
    assert(dc.remainder(dc.top, three) === dc.top)
    assert(dc.remainder(twoZone, dc.top) === dc.top)
    /* BOTTOM < X < TOP */
    assert(dc.remainder(nine, three) === threeZzero)
    assert(dc.remainder(twoZone, twoZzero) === twoZone)
    assert(dc.remainder(twoZone, nine) === dc.top)
    assert(dc.remainder(twoZzero, eightZzero) === twoZzero)
  }


  "CongruenceDomainCore.lub" should
    " - return the least upper bound of the two congruences given as input" in {
    /* BOTTOM */
    assert(dc.lub(dc.bottom, dc.bottom) === dc.bottom)
    assert(dc.lub(dc.bottom, three) === three)
    assert(dc.lub(nineZone, dc.bottom) === nineZone)
    /* TOP */
    assert(dc.lub(dc.top, dc.top) === dc.top)
    assert(dc.lub(dc.top, three) === dc.top)
    assert(dc.lub(twoZone, dc.top) === dc.top)
    /* BOTTOM < X < TOP */
    assert(dc.lub(three, three) === three)
    assert(dc.lub(eightZtwo, twoZzero) === twoZzero)
    assert(dc.lub(twoZone, zero) === dc.top)
    assert(dc.lub(threeZzero, nine) === threeZzero)
  }
} // end of CongruenceDomainCoreSuite