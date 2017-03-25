package it.unipd.jandom.domains.numerical.sign
import Sign._

/**
  * The elements of the extended sign domain
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object ESeq {
  // numbers greater or equal than 0 (>= 0)
  case object Geq0 extends Sign {
    override def toString: String = "\u2265 0"
  }
  // numbers less or equal than 0 (<= 0)
  case object Leq0 extends Sign {
    override def toString: String = "\u2264 0"
  }
  // numbers not equal to 0 (!= 0)
  case object Neq0 extends Sign {
    override def toString: String = "\u2260 0"
  }
} // end of ESeq object
