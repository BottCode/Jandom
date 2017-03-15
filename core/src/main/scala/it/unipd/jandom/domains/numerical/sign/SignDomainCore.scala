package it.unipd.jandom.domains.numerical.sign

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}

trait Sign
// positive numbers (> 0)
case object Plus extends Sign
// negative numbers (< 0)
case object Minus extends Sign
// null numbers (= 0)
case object Zero extends Sign
// no accurate info available for variable
case object SignTop extends Sign
// no possible value
case object SignBottom extends Sign

/**
  * Operations on the (basic) sign domain.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object SignDomainCore extends CompleteLatticeOperator[Sign] with IntOperator[Sign] with Abstraction[Int, Sign] {

  /**
    * Factory method for signs (i.e. abstraction).
    *
    * @param n number that has to be converted to sign
    * @return sign of `n`
    */
  override def alpha(n : Int) : Sign =
    if(n < 0)
      Minus
    else if(n == 0)
      Zero
    else
      Plus

  /**
    * Factory method for signs.
    *
    * @param num number that has to be converted to sign
    * @return sign of `num`
    */
  // TODO: Stale - keep or dispose?
  def toSign(num : Double): Sign =
    if(num > 0)
      Plus
    else if(num < 0)
      Minus
    else
      Zero

  /**
    * Returns the sum of two sign variables.
    *
    * @param s the first term of the addition
    * @param t the second term of the addition
    * @return the result of the addition
    */
  def sum(s: Sign, t: Sign) : Sign =
    (s,t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (SignTop, _) => SignTop
      case (_, SignTop) => SignTop
      case (a, Zero) => a
      case (Zero, a) => a
      case (Plus, Plus) => Plus
      case (Minus, Minus) => Minus
      case _ => SignTop
    }

  /**
    * Returns the multiplication of two sign variables.
    *
    * @param s the first factor of the multiplication
    * @param t the second factor of the multiplication
    * @return the result of the multiplication
    */
  def mult(s: Sign, t : Sign) : Sign =
    (s,t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (_, Zero) => Zero
      case (Zero, _) => Zero
      case (SignTop, _) => SignTop
      case (_, SignTop) => SignTop
      case (a, b) => if(a == b) Plus else Minus
    }

  /**
    * Performs the (-) prefix operation.
    *
    * @param s sign that will be inverted
    * @return the inverse of `s`
    */
  def inverse(s: Sign) : Sign =
    s match {
      case Plus => Minus
      case Minus => Plus
      case a => a
    }

  /**
    * Returns the division of two sign variables.
    *
    * @param s the numerator of the division
    * @param t the denominator of the division
    * @return the result of the division
    */
  def division(s : Sign, t : Sign) : Sign =
    (s, t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (_, Zero) => SignBottom
      case (SignTop, _) => SignTop
      case (Zero, _) => Zero
      case (_, SignTop) => SignTop
      case _ => SignTop
    }

  /**
    * Returns the result of the modulo operation between two sign variables.
    *
    * @param s number put under modulo operation
    * @param t modulus
    * @return the remainder of the modulo operation
    */
  def remainder(s : Sign, t : Sign) : Sign =
    (s,t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (_, Zero) => SignBottom
      case (Zero, _) => Zero
      case (_, _) => SignTop
    }

  /**
    * Returns the least upper bound between two sign variables.
    *
    * @param s first term of the lub
    * @param t second term of the lub
    * @return the lub of `s` and `t`
    */
  def lub(s : Sign, t : Sign) : Sign =
    (s, t) match {
      case (SignTop, _) => SignTop
      case (_, SignTop) => SignTop
      case (SignBottom, a) => a
      case (a, SignBottom) => a
      case (a, b) => if (a == b) a else SignTop
    }

  /**
    * Returns the greatest lower bound between two sign variables.
    *
    * @param s first term of the glb
    * @param t second term of the glb
    * @return the glb of `s` and `t`
    */
  def glb(s : Sign, t : Sign) : Sign =
      (s,t) match {
        case (SignTop, a) => a
        case (a, SignTop) => a
        case (_, SignBottom) => SignBottom
        case (SignBottom, _) => SignBottom
        case (a, b) => if(a == b) a else SignBottom
      }

  /**
    * Performs a Java-like comparison (same behaviour as Java's compareTo) between two signs.
    *
    * @param s left hand side
    * @param t right hand side
    * @return 1 if `s` > `t` -- 0 if `s` = `t` -- -1 if `s` < `t`
    */
  def compare(s: Sign, t:Sign): Option[Int] =
    (s, t) match {
      case (SignTop, SignTop) => Option(0)
      case (SignTop, _) => Option(1)
      case (_, SignTop) => Option(-1)
      case (SignBottom, SignBottom) => Option(0)
      case (SignBottom, _) => Option(-1)
      case (_, SignBottom) => Option(1)
      case (a, b) => if (a.equals(b)) Option(0) else Option.empty
    }

  /**
    * Maximum element of this lattice.
    * @return the top element
    */
  override def top: Sign = SignTop

  /**
    * Least element of this lattice.
    * @return the bottom element
    */
  override def bottom: Sign = SignBottom

} // end object SignDomainCore
