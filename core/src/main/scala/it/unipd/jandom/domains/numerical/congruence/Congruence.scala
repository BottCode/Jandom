package it.unipd.jandom.domains.numerical.congruence

/**
  * The elements of the congruence domain.
  * It is a non-relational domain composed of values in the form `aZ + b` (Mod) and bottom (CongruenceBottom)
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
    * @param a modulus
    * @param b offset
    */
  case class Mod(a : Option[Int], b : Int) extends Congruence {
    /**Checks if `a` belongs to the set `N* U {∞}` as described in
      [[it.unipd.jandom.domains.numerical.congruence.CongruenceDomainCore]] */
    require(
      a match {
        case None => true
        case Some(x) => x > 0
      })

    /**
      * @inheritdoc
      */
    override def toString: String = {
      var s : String = "\u2208 "
      (a, b) match {
        case (None, 0) => s + "{0}"
        case (None, _) => s + "{" + b + "}"
        case (Some(1), 0) => s + "\u2124"
        case (Some(1), _) => s + "\u2124 + " + b
        case (Some(x), 0) => s + x + "\u2124"
        case (Some(x), _) => s + x + "\u2124 + " + b
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
    override def toString: String = "= \u22A5"
  }

} // end of Congruence object
