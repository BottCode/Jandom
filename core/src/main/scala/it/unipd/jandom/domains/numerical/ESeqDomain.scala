package it.unipd.jandom.domains.numerical

import breeze.linalg.sum
import it.unich.jandom.domains.numerical.LinearForm

/**
  * Extended sign domain, i.e. the domain composed of the elements Minus (negative numbers), Plus (positive numbers),
  * Zero (0), Geq0 (>=0), Leq0 (<=0) and Neq0 (!=0). SignTop and SignBottom complete the lattice, providing a greatest
  * and a least element for this set.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>,
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  */
class ESeqDomain extends BaseNumericalDomain[Sign, ESeqDomainCore.type](ESeqDomainCore) {


  override def createProperty(elements: Array[Sign], unreachable: Boolean): ESeqDomain.this.type = ???

  class Property (sign : Array[Sign], unreachable : Boolean) extends BaseProperty(sign, unreachable) {

    /** @inheritdoc
      */
    override def linearInequality(lf: LinearForm) : Property = {
      val s : Sign = linearEvaluation(lf)
      if (isEmpty)
        return this
      s match {
        case Plus => bottom
        case SignTop => top
        case SignBottom => bottom
        case _ => this
      }
    }

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm) : Property = {
      if (isEmpty)
        return this
      val s : Sign = linearEvaluation(lf)
      s match {
        case Plus => this
        case Minus => this
        case Zero => bottom
        case SignTop => top //lub(Plus, Minus)
        case SignBottom => bottom
      }
    }
  } // end class Property
} // end class ESeqDomain

object ESeqDomain {
  def apply() = new ESeqDomain()
} // end of SignDomain's companion object
