package it.unipd.jandom.domains.numerical.congruence

/**
  * The elements of the congruence domain.
  * It is a non-relational domain composed of values in the form `aZ + b` (Mod)
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object Congruence {
  trait Congruence
  case class Mod(a : Option[Int], b : Int) extends Congruence {
    // Checking if the value is bigger than 0
    require(
      a match {
        case None => true
        case Some(x) => x > 0
      })

    override def toString: String = {
      (a, b) match {
        case (None, 0) => "0"
        case (None, _) => b + ""
        case (Some(1), 0) => "Z"
        case (Some(1), _) => "Z + " + b
        case (Some(a), 0) => a + "Z"
        case (Some(a), _) => a + "Z + " + b
      }
    }
  }
  // Corresponds to the empty set
  case object CongruenceBottom extends Congruence {  
    override def toString: String = "\u22A5"
  }

}
