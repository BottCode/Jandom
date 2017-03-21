package it.unipd.jandom.domains.numerical.constant

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.BaseNumericalDomain
import it.unipd.jandom.domains.numerical.constant.Constant._

/**
  * Constant domain, i.e. the domain composed of constant values. 
  * ConstantTop and ConstantBottom complete the lattice, providing
  * a greatest and a least element for this set.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ConstantDomain extends BaseNumericalDomain[Constant, ConstantDomainCore](ConstantDomainCore()) {

  /**
    * @inheritdoc
    */
  override def createProperty(constants: Array[Constant], unreachable: Boolean): Property =
    new Property(constants, unreachable)

  /**
    * Numerical property that tells whether the variables in a certain point of the CFG are constant or not.
    * @param constants array of the variables' constant status
    * @param unreachable tells if a given program point is unreachable
    */
  class Property (constants : Array[Constant], unreachable: Boolean) extends BaseProperty(constants, unreachable) {

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm): Property = {
      if (isEmpty)
        return this
      val constant : Constant = linearEvaluation(lf)
      constant match {
        case ConstantBottom => bottom
        case Const(0) => bottom
        case _ => this // constant != Const(0)
      }
    }

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm): Property = {
      val constant: Constant = linearEvaluation(lf)
      if (isEmpty)
        return this
      constant match {
        case ConstantBottom => bottom
        case Const(c) => if (c > 0) bottom else this
        case _ => this
      }
    }

    /**
      * @inheritdoc
      */
    override def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (unreachable)
        "[ empty ]"
      else {
        val bounds = for (i <- 0 until dimension) yield {
          val c = constants(i) match {
            case ConstantTop => "\u22A4"
            case ConstantBottom => "\u22A5"
            case Const(num) => num
          }
          s"${vars(i)} = $c"
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }

  } // end of Property
} // end of ConstantDomain (class)

object ConstantDomain {
  /**
    * Factory method of ConstantDomain
    */
  def apply() = new ConstantDomain()
} // end of ConstantDomain (companion object)
