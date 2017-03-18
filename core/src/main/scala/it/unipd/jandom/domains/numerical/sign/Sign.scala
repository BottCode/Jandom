package it.unipd.jandom.domains.numerical.sign

object Sign {
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
}
