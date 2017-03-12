package it.unipd.jandom.domains.numerical

/**
  * Utilities for the domain <0, 0, 1 and >1.
  * This domain is intended to be used for analysis on integer variables.
  */
object ExtendedSigns01Core {
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
}
