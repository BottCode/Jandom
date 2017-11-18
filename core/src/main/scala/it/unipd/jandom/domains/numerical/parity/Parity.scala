package it.unipd.jandom.domains.numerical.parity

object Parity {
  sealed trait Parity
  // multiples of 2
  case object Even extends Parity
  // multiples of 2 plus 1
  case object Odd extends Parity
  // no accurate info available for variable
  case object ParityTop extends Parity
  // no possible value
  case object ParityBottom extends Parity
} // end of Parity object
