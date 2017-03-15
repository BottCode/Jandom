package it.unipd.jandom.domains.numerical.sign

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.BaseNumericalDomain

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


  override def createProperty(elements: Array[Sign], unreachable: Boolean): Property =
    new Property(elements, unreachable)

  class Property (sign : Array[Sign], unreachable : Boolean) extends BaseProperty(sign, unreachable) {
    /**
      * Compute the minimum and maximum value of a linear form in a box.
      *
      * @param lf a linear form
      * @return  the sign of the linear evaluation of `lf`
      */
    override def linearEvaluation(lf: LinearForm): Sign = {
      val known = lf.known.toDouble
      val homcoeffs = lf.homcoeffs.map (_.toDouble).toArray
      linearEvaluation(known, homcoeffs)
    }

    /**
      * Compute the minimum and maximum value of a linear form in a box.
      *
      * @param known the known term of a linear form
      * @param homcoeffs homogeneous coefficients of a linear form
      * @return the sign of the linear evaluation of a linear form
      */
    def linearEvaluation(known: Double, homcoeffs: Array[Double]): Sign = {
      require(homcoeffs.length <= dimension)
      var s: Sign = ESeqDomainCore.alpha(known.toInt)
      if (unreachable && homcoeffs.exists { _ != 0 }) return SignTop
      for (i <- homcoeffs.indices) {
        if (homcoeffs(i) > 0) {
          val t: Sign = sign(i)
          s = ESeqDomainCore.sum(s, t)
        }
        else if (homcoeffs(i) < 0) {
          val t: Sign = ESeqDomainCore.inverse(sign(i))
          s = ESeqDomainCore.sum(s, t)
        }
      }
      s
    }

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
