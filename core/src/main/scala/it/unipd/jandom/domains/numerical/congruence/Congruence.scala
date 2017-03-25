package it.unipd.jandom.domains.numerical.congruence

/**
  * The elements of the congruence domain.
  * It is a non-relational domain composed of values in the form `aZ + b` (Mod) and bottom (CongruenceBottom)
  *
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object Congruence {
  trait Congruence

  /**
    * Represents the elements of the domain of the form aZ + b.
    * Invariant: a = None (infinite) or a = Some(x) with x > 0
    * @param a
    * @param b
    */
  case class Mod(a : Option[Int], b : Int) extends Congruence {
    /**Checks if `a` belongs to the set `N* U {inf}`      */
    require(
      a match {
        case None => true
        case Some(x) => x > 0
      })

    /**
      * @inheritdoc
      */
    override def toString: String = {
      (a, b) match {
        case (None, 0) => "0"
        case (None, _) => b + ""
        case (Some(1), 0) => "Z"
        case (Some(1), _) => "Z + " + b
        case (Some(x), 0) => x + "Z"
        case (Some(x), _) => x + "Z + " + b
      }
    }
  }

  /**
    * Corresponds to the bottom of the Congruence lattice
    */
  case object CongruenceBottom extends Congruence {
    /**
      * @inheritdoc
      */
    override def toString: String = "\u22A5"
  }

}
