package it.unipd.jandom.domains.numerical

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}

// numbers greater or equal than 0 (>= 0)
case object Geq0 extends Sign
// numbers less or equal than 0 (>= 0)
case object Leq0 extends Sign
// numbers not equal to 0 (>= 0)
case object Neq0 extends Sign

/**
  * Operations on the extended domain of signs with >=0, <=0 and !=0.
  */
object ESeqDomainCore extends CompleteLatticeOperator[Sign] with IntOperator[Sign] with Abstraction[Int, Sign] {

  override def alpha(num: Int): Sign =
    if(num < 0)
      Minus
    else if(num == 0)
      Zero
    else
      Plus

  /**
    * Factory method for signs.
    *
    * @param num number that has to be converted to sign
    * @return sign of `num`
    */
  // stale
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
      case (Geq0, Geq0) => Geq0
      case (Geq0, Plus) => Plus
      case (Plus, Geq0) => Plus
      case (Leq0, Leq0) => Leq0
      case (Leq0, Minus) => Minus
      case (Minus, Leq0) => Minus
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
      case (Plus, a) => a
      case (a, Plus) => a
      case (Minus, a) => inverse(a)
      case (a, Minus) => inverse(a)
      case (Leq0, Geq0) => Leq0
      case (Geq0, Leq0) => Leq0
      case _ => SignTop
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
      case Leq0 => Geq0
      case Geq0 => Leq0
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
      case (Plus, Minus) => Leq0
      case (Minus, Plus) => Leq0
      case (Plus, Geq0) => Geq0
      case (Geq0, Plus) => Geq0
      case (Minus, Geq0) => Leq0
      case (Geq0, Minus) => Leq0
      case (Plus, Leq0) => Leq0
      case (Leq0, Plus) => Leq0
      case (Minus, Leq0) => Geq0
      case (Leq0, Minus) => Geq0
      case (Leq0, Geq0) => Leq0
      case (Geq0, Leq0) => Leq0
        // TODO: Check if something is missing
      case (a, b) => if (a.equals(b)) Geq0 else SignTop
    }

  /**
    * Returns the result of the modulo operation between two sign variables.
    *
    * @param s number put under modulo operation
    * @param t modulus
    * @return the remainder of the modulo operation
    */
  def remainder(s : Sign, t : Sign) : Sign =
    (s, t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (_, Zero) => SignBottom
      case (Zero, _) => Zero
      case (Plus, _) => Geq0
      case (Geq0, _) => Geq0
      case (Minus, _) => Leq0
      case (Leq0, _) => Leq0
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
      case (Zero, Plus) => Geq0
      case (Plus, Zero) => Geq0
      case (Zero, Minus) => Leq0
      case (Minus, Zero) => Leq0
      case (Plus, Minus) => Neq0
      case (Minus, Plus) => Neq0
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
    (s, t) match {
      case (SignTop, a) => a
      case (a, SignTop) => a
      case (_, SignBottom) => SignBottom
      case (SignBottom, _) => SignBottom
      case (Geq0, Leq0) => Zero
      case (Leq0, Geq0) => Zero
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
      case (Zero, Geq0) => Option(-1)
      case (Plus, Geq0) => Option(-1)
      case (Zero, Leq0) => Option(-1)
      case (Minus, Leq0) => Option(-1)
      case (Plus, Neq0) => Option(-1)
      case (Minus, Neq0) => Option(-1)
      case (Geq0, Zero) => Option(1)
      case (Geq0, Plus) => Option(1)
      case (Leq0, Zero) => Option(1)
      case (Leq0, Minus) => Option(1)
      case (Neq0, Plus) => Option(1)
      case (Neq0, Minus) => Option(1)
      case (a, b) => if (a.equals(b)) Option(0) else Option.empty
    }

  override def top: Sign = SignTop
  override def bottom: Sign = SignBottom
} // end object ESeqDomainCore
