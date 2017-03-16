package it.unipd.jandom.domains.numerical.sign

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.{LinearForm, NumericalDomain, NumericalProperty}
import it.unich.jandom.utils.numberext.RationalExt
import it.unipd.jandom.domains.numerical.sign.ES01._
import spire.math.Rational

/**
  * Sign domain extended with the 0 and 1 constants.
  */
object ExtendedSigns01Domain extends NumericalDomain {

  import ExtendedSigns01DomainCore._

  case class Property private[ExtendedSigns01Domain](sign : Array[ExtendedSign01], unreachable : Boolean) extends NumericalProperty[Property] {
    val dimension: Int = sign.length
    require(dimension >= 0)

    type Domain = ExtendedSigns01Domain.type

    def domain = ExtendedSigns01Domain

    def tryCompareTo[B >: Property](that: B)(implicit evidence$1: (B) => PartiallyOrdered[B]): Option[Int] = that match {

      case that: Property =>
        require(dimension == that.dimension)
        (isEmpty, that.isEmpty) match {
          case (true, true) => Option(0)
          case (false, true) => Option(1)
          case (true, false) => Option(-1)
          case (false, false) =>
            println(s"s: $this   --- t: $that")
            val signPairs = (this.sign, that.sign).zipped
            val comparisonList = signPairs.map(compare)

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

    override def union(that: Property): Property =
      Property((this.sign, that.sign).zipped.map(lub), unreachable && that.unreachable)

    override def intersection(that: Property) : Property =
      ExtendedSigns01Domain.this ((this.sign, that.sign).zipped.map(glb))

    override def widening(that: Property): Property = union(that)

    override def narrowing(that: Property): Property = this

    override def nonDeterministicAssignment(n: Int): Property =
      if (unreachable)
        this
      else
        Property(sign.updated(n, ES01Top), unreachable = false)

    private def linearEvaluation(lf : LinearForm) : ExtendedSign01 = {
      val known = lf.known.toDouble.toInt
      val homCoeffs =lf.homcoeffs.map (_.toDouble.toInt).toArray
      linearEvaluation(known, homCoeffs)
    }

    private def linearEvaluation(known : Int, homCoeffs : Array[Int]) : ExtendedSign01 = {
      require(homCoeffs.length <= dimension)
      var s : ExtendedSign01 = alpha(known)
      if  (unreachable && homCoeffs.exists { _ != 0} ) return ES01Top
      for (i <- homCoeffs.indices) {
        if (homCoeffs(i) > 0) {
          val t: ExtendedSign01 = sign(i)
          s = sum(s, t)
        } else if (homCoeffs(i) < 0) {
          val t: ExtendedSign01 = inverse(sign(i))
          s = sum(s, t)
        }
      }
      s
    }

    def linearAssignment(pos : Int, lf : LinearForm) : Property = {
      require(pos < sign.length && pos >= 0 && lf.dimension <= dimension)
      if(unreachable)
        return this
      val s : ExtendedSign01 = linearEvaluation(lf)
      ExtendedSigns01Domain.this (sign.updated(pos, s))
    }

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
        case ES01Top => top
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
        case One => this
        case GTOne => this
        case Negative => this
        case ES01Top => top
        case ES01Bottom => bottom
      }
    }

    def minimize(lf: LinearForm): RationalExt =
      if (lf.homcoeffs.exists(!_.isZero))
        RationalExt.NegativeInfinity
      else
        RationalExt(lf.known)

    def maximize(lf: LinearForm): RationalExt =
      if (lf.homcoeffs.exists(!_.isZero))
        RationalExt.PositiveInfinity
      else
        RationalExt(lf.known)

    def frequency(lf: LinearForm): Option[Rational] =
      if (lf.homcoeffs.exists(!_.isZero))
        Option.empty
      else
        Option(lf.known)

    def constraints = List()

    def isPolyhedral = false

    /**
      * @inheritdoc
      */
    def addVariable(): Property = {
      if (unreachable)
        return ExtendedSigns01Domain.this.bottom(dimension + 1)
      ExtendedSigns01Domain.this(sign :+ ES01Top)
    }

    /**
      * @inheritdoc
      */
    def delVariable(pos: Int): Property = {
      require(pos < sign.length && pos >= 0)
      val newSign = new Array[ExtendedSign01](sign.length - 1)
      // Copy the first pos-1 elements
      Array.copy(sign, 0, newSign, 0, pos)
      // Copy the remaining elements
      Array.copy(sign, pos + 1, newSign, pos, sign.length - pos - 1)
      Property(newSign, unreachable)
    }

    /**
      * @inheritdoc
      */
    def mapVariables(rho: Seq[Int]): Property = {
      require(rho.length == dimension)
      val newdim = rho.count(_ >= 0)
      require(rho forall { i => i >= -1 && i < newdim })
      // we do not check injectivity
      val newSign = new Array[ExtendedSign01](newdim)
      for ((newi, i) <- rho.zipWithIndex; if newi >= 0)
        newSign(newi) = sign(i)
      Property(newSign, unreachable)
    }

    override def isEmpty : Boolean = unreachable

    override def isTop : Boolean = !isEmpty && sign.forall( _.equals(ES01Top))
    override def isBottom : Boolean = isEmpty

    def bottom : Property = ExtendedSigns01Domain.bottom(sign.length)
    def top : Property = ExtendedSigns01Domain.top(sign.length)

    /**
      * @inheritdoc
    // * @throws IllegalArgumentException
      */
    def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (unreachable)
        "empty"
      else {
        val bounds = for (i <- 0 until dimension) yield {
          val h = sign(i) match {
            case ES01Top => "TOP"
            case ES01Bottom => "BOTTOM"
            case GTOne => "Gt1"
            case Negative => "NEG"
            case Zero => "Zero"
            case One => "One"
          }
          s"${vars(i)} = $h"
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }

    /**
      * @inheritdoc
      */
    override def variableMul(n: Int, m: Int): Property = {
      sign(n) = mult(sign(n), sign(m))
      this
    }

    /**
      * @inheritdoc
      */
    override def variableDiv(n : Int, m : Int): Property = {
      sign(n) = division(sign(n), sign(m))
      this
    }

    /**
      * @inheritdoc
      */
    override def variableNeg(n: Int = dimension - 1): Property = {
      sign(n) = inverse(sign(n))
      this
    }

    /**
      * @inheritdoc
      */
    override def variableRem(n: Int = dimension - 2, m: Int = dimension - 1): Property = {
      sign(n) = remainder(sign(n), sign(m))
      this
    }

  } // end Property

  def apply(signArray : Array[ExtendedSign01]) : Property =
    Property(signArray, signArray.forall( _.equals(ES01Bottom)))

  val widenings = Seq(WideningDescription.default[Property])

  def top(n: Int) = Property(Array.fill(n)(ES01Top), unreachable = false)
  def bottom(n: Int) = Property(Array.fill(n)(ES01Bottom), unreachable = true)

}
