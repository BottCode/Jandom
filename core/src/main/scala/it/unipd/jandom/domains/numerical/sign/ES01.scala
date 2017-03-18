package it.unipd.jandom.domains.numerical.sign

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