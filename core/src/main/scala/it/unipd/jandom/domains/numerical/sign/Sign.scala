package it.unipd.jandom.domains.numerical.sign

/**
  * The elements of the sign domain
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
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
