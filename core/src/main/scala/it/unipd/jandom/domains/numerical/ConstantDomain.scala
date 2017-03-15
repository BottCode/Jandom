package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.numerical.{LinearForm}
import ConstantDomainCore._
import it.unipd.jandom.domains.numerical


/**
  * Constant domain
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>, Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *           Stefano Munari <stefano.munari.1@studenti.unipd.it>
  */
class ConstantDomain extends BaseNumericalDomain[Constant, numerical.ConstantDomainCore.type](numerical.ConstantDomainCore) {


  override def createProperty(constants: Array[Constant], unreachable: Boolean): Property =
    new Property(constants, unreachable)

  class Property (constants : Array[Constant], unreachable: Boolean) extends BaseProperty(constants, unreachable) {

    def apply(constants: Array[Constant], unreachable: Boolean) : Property = new Property(constants, unreachable)

    /**
      * @inheritdoc
      */
    override def linearEvaluation(lf: LinearForm): Constant = {
      val known = lf.known.toInt
      val homcoeffs = lf.homcoeffs.map (_.toInt).toArray
      linearEvaluation(known, homcoeffs)
    }

    /**
      * @inheritdoc
      */
    override def linearEvaluation(known: Int, homcoeffs: Array[Int]): Constant = {
      require(homcoeffs.length <= dimension)
      var constant: Constant = alpha(known)
      if (unreachable && homcoeffs.exists { _ != 0 })
        return ConstantTop
      for (i <- homcoeffs.indices) {
        if (homcoeffs(i) > 0) {
          val t: Constant = constants(i)
          constant = sum(constant, t)
        }
        else if (homcoeffs(i) < 0) {
          val t: Constant = inverse(constants(i))
          constant = sum(constant, t)
        }
      }
      constant
    }

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
        case ConstantTop => top // lub(Const)
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
        case ConstantTop => top
        case Const(c) => if (c > 0) bottom else this
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
            case ConstantTop => "TOP"
            case ConstantBottom => "BOTTOM"
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
    * Returns an abstract domain for boxes which is correct w.r.t. real arithmetic or
    * double arithmetic, according to the parameter `overReals`.
    */
  def apply() = new ConstantDomain()

} // end of ConstantDomain (companion object)
