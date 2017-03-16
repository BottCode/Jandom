package it.unipd.jandom.domains.numerical.sign

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}

object ES01 {
  trait ExtendedSign01
  // negative numbers (< 0)
  case object Negative extends ExtendedSign01
  // null numbers (= 0)
  case object Zero extends ExtendedSign01
  // numbers equal to 1 (= 1)
  case object One extends ExtendedSign01
  // numbers greater than 1 (> 1)
  case object GTOne extends ExtendedSign01
  // no accurate info available for variable
  case object ES01Top extends ExtendedSign01
  // no possible value
  case object ES01Bottom extends ExtendedSign01
}

import ES01._

/**
  * Operations on the sign domain extended with the element 1.
  * This domain is intended to be used for analysis on integer variables.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object ExtendedSigns01DomainCore extends Abstraction[Int, ExtendedSign01]
  with IntOperator[ExtendedSign01] with CompleteLatticeOperator[ExtendedSign01] {

  /**
    * @inheritdoc
    */
  override def alpha(n : Int) : ExtendedSign01 = {
    if (n == 0) return Zero
    if (n == 1) return One
    if (n < 0)  return Negative
    GTOne
  }

  /**
    * @inheritdoc
    */
  def sum(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (ES01Top, _) => ES01Top
      case (_, ES01Top) => ES01Top
      case (a, Zero) => a
      case (Zero, a) => a
      case (One, One) => GTOne
      case (GTOne, One) => GTOne
      case (One, GTOne) => GTOne
      case (GTOne, GTOne) => GTOne
      case _ => ES01Top
    }
  }

  /**
    * @inheritdoc
    */
  def mult(s : ExtendedSign01, t : ExtendedSign01): ExtendedSign01 = {
    (s, t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (a, Zero) => Zero
      case (Zero, a) => Zero
      case (a, One) => a
      case (One, a) => a
      case (Negative, Negative) => ES01Top
      case (Negative, GTOne) => Negative
      case (GTOne, Negative) => Negative
      case (GTOne, GTOne) => GTOne
      case _ => ES01Top
    }
  }

  /**
    * @inheritdoc
    */
    def inverse(s : ExtendedSign01): ExtendedSign01 = {
      s match {
        case GTOne => Negative
        case One => Negative
        case Negative => ES01Top
        case a => a
      }
  }

  /**
    * @inheritdoc
    */
  def division(s : ExtendedSign01, t : ExtendedSign01): ExtendedSign01 = {
    (s,t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (_, Zero) => ES01Bottom
      // we already took care of a / 0
      case (Zero, _) => Zero
        // a / 1 = a
      case (a, One) => a
      case (One, GTOne) => Zero
      // (2 / 5) = 0   ---   (5 / 5) = 1   ---   (10 / 5) = 2
      case (_, GTOne) => ES01Top
      // (-2 / -5) = 0   ---   (-4 / -2) = 2
      // (1 / -5) = 0   ---   (1 / -1) = -1
      // (5 / -5) = -1   ---   (2 / -5) = 0
      case (_, Negative) => ES01Top
      case _ => ES01Top
    }
  }

  /**
    * @inheritdoc
    */
  def remainder(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (_, Zero) => ES01Bottom
      case (_, One) => Zero
      case (Zero, _) => Zero
      case (One, GTOne) => One
      case _ => ES01Top
    }
  }

  /**
    * @inheritdoc
    */
  def lub(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Top, _) => ES01Top
      case (_, ES01Top) => ES01Top
      case (a, ES01Bottom) => a
      case (ES01Bottom, a) => a
      case (a, b) => if (a.equals(b)) a else ES01Top
    }
  }

  /**
    * @inheritdoc
    */
  def glb(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Top, a) => a
      case (a, ES01Top) => a
      case (_, ES01Bottom) => ES01Bottom
      case (ES01Bottom, _) => ES01Bottom
      case (a,b) => if(a.equals(b)) a else ES01Bottom
    }
  }

  /**
    * @inheritdoc
    */
  def compare(s : ExtendedSign01, t : ExtendedSign01) : Option[Int] = {
    (s,t) match {
      case (ES01Top, ES01Top) => Option(0)
      case (ES01Bottom, ES01Bottom) => Option(0)
      case (ES01Top, _) => Option(1)
      case (_, ES01Top) => Option(-1)
      case (ES01Bottom, _) => Option(-1)
      case (_, ES01Bottom) => Option(1)
      case (a,b) => if(a.equals(b)) Option(0) else Option.empty
    }
  }

  /**
    * @inheritdoc
    */
  override def top: ExtendedSign01 = ES01Top

  /**
    * @inheritdoc
    */
  override def bottom: ExtendedSign01 = ES01Bottom
} // end object ExtendedSigns01DomainCore
