package it.unipd.jandom.domains.numerical.sign

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.BaseNumericalDomain
import it.unipd.jandom.domains.numerical.sign.ES01._

/**
  * Extended sign domain with 0 and 1, i.e. the domain composed of the elements Negative (negative numbers), GTOne
  * (numbers greater than one), Zero (0) and One (1). SignTop and SignBottom complete the lattice, providing a greatest
  * and a least element for this set.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ExtendedSigns01Domain
  extends BaseNumericalDomain[ExtendedSign01, ExtendedSigns01DomainCore](ExtendedSigns01DomainCore()) {

  // this class uses the operations defined in ExtendedSign01DomainCore

  /**
    * @inheritdoc
    */
  override def createProperty(signsArray : Array[ExtendedSign01], unreachable : Boolean): Property =
    Property(signsArray, unreachable)

  /**
    * Numerical property that describes the extended sign with 0,1 of the variables in a certain point of the CFG.
    * @param sign array of the variables' signs
    * @param unreachable tells if a given program point is unreachable
    */
  case class Property private[ExtendedSigns01Domain](sign : Array[ExtendedSign01], override val unreachable : Boolean)
    extends BaseProperty(sign, unreachable) {

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm) : Property = {
      val s : ExtendedSign01 = linearEvaluation(lf)
      if (isEmpty)
        return this
      s match {
        case One => bottom
        case GTOne => bottom
        case ES01Bottom => bottom
        case _ => this
      }
    }

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm) : Property = {
      if (isEmpty)
        return this
      val s : ExtendedSign01 = linearEvaluation(lf)
      s match {
        case Zero => bottom
        case ES01Bottom => bottom
        case _ => this
      }
    }

  } // end of ES01Domain's Property
} // end of ES01Domain's class

object ExtendedSigns01Domain {
  /**
    * Ctor for ExtendedSigns01Domain
    */
  def apply(): ExtendedSigns01Domain = new ExtendedSigns01Domain()
} // end of ES01Domain's companion object
