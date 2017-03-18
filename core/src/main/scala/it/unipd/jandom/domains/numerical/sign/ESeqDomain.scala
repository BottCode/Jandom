package it.unipd.jandom.domains.numerical.sign

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.BaseNumericalDomain
import it.unipd.jandom.domains.numerical.sign.Sign._

/**
  * Extended sign domain, i.e. the domain composed of the elements Minus (negative numbers), Plus (positive numbers),
  * Zero (0), Geq0 (>=0), Leq0 (<=0) and Neq0 (!=0). SignTop and SignBottom complete the lattice, providing a greatest
  * and a least element for this set.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>,
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ESeqDomain extends BaseNumericalDomain[Sign, ESeqDomainCore](ESeqDomainCore()) {

  override def createProperty(elements: Array[Sign], unreachable: Boolean): Property =
    new Property(elements, unreachable)

  type Property = ESeqDomainProperty

  /**
    * Property of a point in the CFG for the ESeq domain.
    * @param sign array of sign variables
    * @param unreachable tells if the point of the CFG is unreachable
    */
  class ESeqDomainProperty (sign : Array[Sign], unreachable : Boolean) extends BaseProperty(sign, unreachable) {

    self: Property =>
    /**
      * Intersection with the half-plane `lf <= 0`.
      *
      */
    override def linearInequality(lf: LinearForm): Property = {
      val s : Sign = linearEvaluation(lf)
      if (isEmpty)
        return this
      s match {
        case Plus => bottom
        case SignBottom => bottom
        case _ => this
      }
    }

    /**
      * Intersection with `lf != 0`.
      *
      */
    override def linearDisequality(lf: LinearForm): Property = {
      if (isEmpty)
        return this
      val s : Sign = linearEvaluation(lf)
      s match {
        case Zero => bottom
        case SignBottom => bottom
        case _ => this
      }
    }
  } // end class Property

} // end class ESeqDomain

object ESeqDomain {
  def apply() = new ESeqDomain()
} // end of ESeqDomain's companion object