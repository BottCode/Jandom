package it.unipd.jandom.domains

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.{LinearForm, NumericalDomain, NumericalProperty}
import it.unich.jandom.utils.numberext.RationalExt
import it.unipd.jandom.domains.ConstantDomainCore._
import spire.math.Rational
import scala.math.PartiallyOrdered

/**
  * Constant domain
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>, Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *           Stefano Munari <stefano.munari.1@studenti.unipd.it>
  */
class ConstantDomain extends NumericalDomain {


  def apply(constants : Array[Constant]): Property = Property(constants, constants.forall( _.equals(ConstantBottom)))

  /**
    * @inheritdoc
    */
  override def top(num: Int) = Property(Array.fill(num)(ConstantTop), unreachable = false)

  /**
    * @inheritdoc
    */
  override def bottom(num: Int) = Property(Array.fill(num)(ConstantBottom), unreachable = true)

  /**
    * @inheritdoc
    */
  override def widenings = Seq(WideningDescription.default[Property])

  case class Property private[ConstantDomain]
    (constants: Array[Constant], unreachable: Boolean) extends NumericalProperty[Property] {

    /**
      * @inheritdoc
      */
    type Domain = ConstantDomain

    /**
      * @inheritdoc
      */
    override def isPolyhedral: Boolean = false

    /**
      * @inheritdoc
      */
    override def constraints : Seq[LinearForm] = List()

    /**
      * @inheritdoc
      */
    override def dimension: Int = constants.length

    /**
      * @inheritdoc
      */
    override def domain = ConstantDomain.this

    /**
      * @inheritdoc
      */
    override def isEmpty: Boolean = unreachable

    /**
      * @inheritdoc
      */
    override def isTop: Boolean = !isEmpty && constants.forall( _.equals(ConstantTop))

    /**
      * @inheritdoc
      */
    override def isBottom: Boolean = isEmpty

    /**
      * @inheritdoc
      */
    override def bottom: Property = ConstantDomain.this.bottom(constants.length)

    /**
      * @inheritdoc
      */
    override def top: Property = ConstantDomain.this.top(constants.length)

    /**
      * @inheritdoc
      */
    override def minimize(lf: LinearForm): RationalExt =
      if (lf.homcoeffs.exists(!_.isZero))
        RationalExt.NegativeInfinity
      else
        RationalExt(lf.known)

    /**
      * @inheritdoc
      */
    override def maximize(lf: LinearForm): RationalExt =
      if (lf.homcoeffs.exists(!_.isZero))
        RationalExt.PositiveInfinity
      else
        RationalExt(lf.known)

    /**
      * @inheritdoc
      */
    override def frequency(lf: LinearForm): Option[Rational] =
      if (lf.homcoeffs.exists(!_.isZero))
        Option.empty
      else
        Option(lf.known)

    /**
      * @inheritdoc
      */
    override def addVariable(): Property = {
      if (unreachable)
        return ConstantDomain.this.bottom(dimension + 1)
      ConstantDomain.this(constants :+ ConstantTop)
    }

    /**
      * @inheritdoc
      */
    override def delVariable(index: Int): Property = {
      require(index < constants.length && index >= 0)
      val result = new Array[Constant](constants.length - 1)
      // Copy the first index-1 elements
      Array.copy(constants, 0, result, 0, index)
      // Copy the remaining elements
      Array.copy(constants, index + 1, result, index, constants.length - index - 1)
      Property(result, unreachable)
    }

    /**
      * @inheritdoc
      */
    override def mapVariables(rho: Seq[Int]): Property =  {
      require(rho.length == dimension)
      val resultDim = rho.count(_ >= 0)
      require(rho forall { i => i >= -1 && i < resultDim })
      // we do not check injectivity
      val result = new Array[Constant](resultDim)
      for ((resultIndex, i) <- rho.zipWithIndex; if resultIndex >= 0)
        result(resultIndex) = constants(i)
      Property(result, unreachable)
    }

    /**
      * @inheritdoc
      * @todo check implementation
      */
    def tryCompareTo[B >: Property](that: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = that match {

      case that: Property =>
        require(dimension == that.dimension)
        (isEmpty, that.isEmpty) match {
          case (true, true) => Option(0)
          case (false, true) => Option(1)
          case (true, false) => Option(-1)
          case (false, false) =>
            val ConstantPairs = (this.constants, that.constants).zipped
            val comparisonList = ConstantPairs.map(compare)

            if (comparisonList.forall {
              case Some(i) => if (i == 0) true else false
              case None => false
            })
              Option(0)

            else if (comparisonList.forall {
              case Some(i) => if (i >= 0) true else false
              case None => false
            } && comparisonList.exists {
              case Some(i) => if (i == 1) true else false
              case None => false
            })
              Option(1)

            else if (comparisonList.forall {
              case Some(i) => if (i <= 0) true else false
              case None => false
            } && comparisonList.exists {
              case Some(i) => if (i == -1) true else false
              case None => false
            })
              Option(-1)

            else
              Option.empty
        }

      case _ => None
    }

    /**
      * @inheritdoc
      * @todo check implementation
      */
    override def union(that: Property): Property =  {
      require(dimension == that.dimension)
      Property((this.constants, that.constants).zipped.map( lub ), this.isEmpty && that.isEmpty)
    }

    /**
      * @inheritdoc
      * @todo check implementation
      */
    override def intersection(that: Property): Property = {
      require(dimension == that.dimension)
      val result = (this.constants, that.constants).zipped.map( glb )
      ConstantDomain.this(result)
    }

    /**
      * @inheritdoc
      * @todo check implementation
      */
    override def narrowing(that: Property): Property = {
      require(dimension == that.dimension)
      Property((this.constants, that.constants).zipped.map(glb), this.isEmpty && that.isEmpty)
    }

    /**
      * @inheritdoc
      * @todo check implementation
      */
    override def widening(that: Property): Property = union(that)

    /**
      * @inheritdoc
      */
    override def mkString(vars: Seq[String]) : String = {
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

    /**
      * @inheritdoc
      */
    override def nonDeterministicAssignment(n: Int): Property = {
      if (unreachable)
        return this
      Property(constants.updated(n, ConstantTop), unreachable = false)
    }

    /**
      * @inheritdoc
      */
    override def linearAssignment(index: Int, lf: LinearForm) : Property = {
      require(index < constants.length && index >= 0 && lf.dimension <= dimension)

      if(unreachable)
        return this
      val constant : Constant = linearEvaluation(lf)

      Property(constants.updated(index, constant), unreachable = false)
    }

    /** @inheritdoc
      * @param lf expression that gets evaluated for the linear inequality
      */
    override def linearInequality(lf: LinearForm) : Property = {
      val constant : Constant = linearEvaluation(lf)
      if (isEmpty)
        return this
      constant match {
        case ConstantBottom => bottom
        case ConstantTop => top
        case Const(c) => if(c > 0) bottom else this
      }
    }

    /**
      * @inheritdoc
      * @param lf expression that gets evaluated for the linear disequality
      */
    override def linearDisequality(lf: LinearForm) : Property = {
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
    override def variableMul(n: Int, m: Int): Property = {
      constants(n) = mult(constants(n), constants(m))
      this
    }

    /**
      * @inheritdoc
      */
    override def variableDiv(n : Int, m : Int): Property = {
      constants(n) = division(constants(n), constants(m))
      this
    }

    /**
      * @inheritdoc
      */
    override def variableNeg(n: Int = dimension - 1): Property = {
      constants(n) = inverse(constants(n))
      this
    }

    /**
    * @todo check implementation
    */
    private def linearEvaluation(lf: LinearForm): Constant = {
      val known = lf.known.toInt
      val homcoeffs = lf.homcoeffs.map (_.toInt).toArray
      linearEvaluation(known, homcoeffs)
    }

    /**
      * @todo check implementation
      */
    private def linearEvaluation(known: Int, homcoeffs: Array[Int]): Constant = {
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
    
  }
}

object ConstantDomain {
  /**
    * Returns an abstract domain for boxes which is correct w.r.t. real arithmetic or
    * double arithmetic, according to the parameter `overReals`.
    */
  def apply() = new ConstantDomain()



}