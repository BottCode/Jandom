package it.unipd.jandom.domains.numerical.sign
import Sign._

object ESeq {
  // numbers greater or equal than 0 (>= 0)
  case object Geq0 extends Sign
  // numbers less or equal than 0 (>= 0)
  case object Leq0 extends Sign
  // numbers not equal to 0 (>= 0)
  case object Neq0 extends Sign
}