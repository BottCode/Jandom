package it.unipd.jandom.domains.numerical

/**
  * Utilities for the domain <0, 0, 1 and >1.
  * This domain is intended to be used for analysis on integer variables.
  */
object ExtendedSigns01DomainCore {
  trait ExtendedSign01
  case object GreaterThanOne extends ExtendedSign01
  case object Negative extends ExtendedSign01
  case object Zero extends ExtendedSign01
  case object One extends ExtendedSign01
  case object ES01Top extends ExtendedSign01
  case object ES01Bottom extends ExtendedSign01

  def toSign(n : Int) : ExtendedSign01 = {
    if (n == 0) Zero
    if (n == 1) One
    if (n < 0)  Negative
    GreaterThanOne
  }

  def sum(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (ES01Top, _) => ES01Top
      case (_, ES01Top) => ES01Top
      case (a, Zero) => a
      case (Zero, a) => a
      case (One, One) => GreaterThanOne
      case (GreaterThanOne, One) => GreaterThanOne
      case (One, GreaterThanOne) => GreaterThanOne
      case (GreaterThanOne, GreaterThanOne) => GreaterThanOne
      case _ => ES01Top
    }
  }

  def mult(s : ExtendedSign01, t : ExtendedSign01) = {
    (s, t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (a, Zero) => Zero
      case (Zero, a) => Zero
      case (a, One) => a
      case (One, a) => a
      case (Negative, Negative) => ES01Top
      case (Negative, GreaterThanOne) => Negative
      case (GreaterThanOne, Negative) => Negative
      case (GreaterThanOne, GreaterThanOne) => GreaterThanOne
      case _ => ES01Top
    }
  }

    def inverse(s : ExtendedSign01) = {
      s match {
        case GreaterThanOne => Negative
        case One => Negative
        case Negative => ES01Top
        case a => a
      }
  }

  def division(s : ExtendedSign01, t : ExtendedSign01) = {
    (s,t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (_, Zero) => ES01Bottom
      // we already took care of a / 0
      case (Zero, _) => Zero
        // a / 1 = a
      case (a, One) => a
      case (One, Negative) => Negative
      case (One, GreaterThanOne) => Zero
      // (2 / 5) = 0   ---   (5 / 5) = 1   ---   (10 / 5) = 2
      case (GreaterThanOne, GreaterThanOne) => ES01Top
      // (-2 / -5) = 0   ---   (-4 / -2) = 2
      // (1 / -5) = 0   ---   (1 / -1) = -1
      // (5 / -5) = -1   ---   (2 / -5) = 0
      case (_, Negative) => ES01Top
    }
  }

  def remainder(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (_, Zero) => ES01Bottom
      case (_, One) => Zero
      case (Zero, _) => Zero
      case (One, GreaterThanOne) => One
      case _ => ES01Top
    }
  }

  def lub(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Top, _) => ES01Top
      case (_, ES01Top) => ES01Top
      case (_, ES01Bottom) => ES01Bottom
      case (ES01Bottom, _) => ES01Bottom
      case (a, b) => if (a.equals(b)) a else ES01Top
    }
  }

  def glb(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Top, a) => a
      case (a, ES01Top) => a
      case (_, ES01Bottom) => ES01Bottom
      case (ES01Bottom, _) => ES01Bottom
      case (a,b) => if(a.equals(b)) a else ES01Bottom
    }
  }

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
}
