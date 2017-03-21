package it.unipd.jandom.domains.numerical.congruence

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.{BaseNumericalDomain}
/**
  * Congruence domain as described in Mine 2002 https://hal.archives-ouvertes.fr/hal-00136663/document.
  * The multiplication and remainder operator was taken and adapted from http://www.dsi.unive.it/~avp/domains.pdf.
  * We based on these works because we could not retrieve the original paper by Granger [1989].
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *
  */
class CongruenceDomain extends BaseNumericalDomain[Congruence, CongruenceDomainCore](CongruenceDomainCore()) {


  override def createProperty(congruences: Array[Congruence], unreachable: Boolean): Property =
    new Property(congruences, unreachable)

  class Property (congruences : Array[Congruence], unreachable: Boolean) extends BaseProperty(congruences, unreachable) {

    def apply(congruences: Array[Congruence], unreachable: Boolean) : Property = new Property(congruences, unreachable)

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm): Property = {
      if (isEmpty)
        return this
      val congruence : Congruence = linearEvaluation(lf)
      congruence match {
        case CongruenceBottom => bottom
        case Mod(None,0) => bottom //The result is the constant 0
        case _ => this // congruence != Const(0)
      }
    }

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm): Property = {
      val congruence: Congruence = linearEvaluation(lf)
      if (isEmpty)
        return this
      congruence match {
        case CongruenceBottom => bottom
        case Mod(None, b) => if (b > 0) bottom else this //The result is a constant
        case _ => this
      }
    }



  } // end of Property

} // end of CongruenceDomain (class)

object CongruenceDomain {

  /**
    * Returns an abstract domain for boxes which is correct w.r.t. real arithmetic or
    * double arithmetic, according to the parameter `overReals`.
    */
  def apply() = new CongruenceDomain()

} // end of CongruenceDomain (companion object)
