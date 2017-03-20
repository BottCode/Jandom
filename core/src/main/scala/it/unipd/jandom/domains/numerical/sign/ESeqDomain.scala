package it.unipd.jandom.domains.numerical.sign

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.sign.Sign._
import it.unipd.jandom.domains.numerical.sign.ESeq._

/**
  * Extended sign domain, i.e. the domain composed of the elements Minus (negative numbers), Plus (positive numbers),
  * Zero (0), Geq0 (>=0), Leq0 (<=0) and Neq0 (!=0). SignTop and SignBottom complete the lattice, providing a greatest
  * and a least element for this set.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>,
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ESeqDomain extends SignDomain {

  override def createProperty(elements: Array[Sign], unreachable: Boolean): Property =
    new Property(elements, unreachable)

  /**
    * Property of a point in the CFG for the ESeq domain.
    * @param sign array of sign variables
    * @param unreachable tells if the point of the CFG is unreachable
    */
  class ESeqDomainProperty (sign : Array[Sign], unreachable : Boolean) extends SignProperty(sign, unreachable) {

    self: Property =>
    
  } // end class Property
} // end class ESeqDomain

object ESeqDomain {
  def apply() = new ESeqDomain()
} // end of ESeqDomain's companion object
