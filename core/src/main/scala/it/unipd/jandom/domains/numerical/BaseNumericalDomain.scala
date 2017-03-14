package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.{LinearForm, NumericalDomain, NumericalProperty}
import it.unich.jandom.utils.numberext.RationalExt
import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import spire.ClassTag
import spire.math.Rational

import scala.math.PartiallyOrdered

/**
  * @note CoreType is the trait of the XDomainCore object (e.g. Sign, Constant, etc.)
  */
abstract class BaseNumericalDomain
  [T : ClassTag, CoreType <: CompleteLatticeOperator[T] with IntOperator[T] with Abstraction[Int, T]](core : CoreType)
  extends NumericalDomain {

  def createProperty(elements: Array[T], unreachable: Boolean) : Property

  def createProperty(elements: Array[T]) : Property =
    createProperty(elements, elements.forall(x => x.equals(core.bottom)))

  /**
    * @inheritdoc
    */
  def bottom(n: Int)  = createProperty(Array.fill(n)(core.bottom))

  /**
    * @inheritdoc
    */
  def top(n: Int)  = createProperty(Array.fill(n)(core.top))

  /**
    * @inheritdoc
    */
    val widenings = Seq(WideningDescription.default[Property])

  /**
    * @inheritdoc
    * For numerical domains, properties needs to be instances of
    */
  abstract class BaseProperty (val elements: Array[T], val unreachable: Boolean) extends NumericalProperty[Property] {

    self: Property =>

    /**
      * @inheritdoc
      */
    def dimension: Int = elements.length

    /**
      * @inheritdoc
      */
    override def constraints: Seq[LinearForm] = List()

    /**
      * @inheritdoc
      */
    def bottom : Property = createProperty(Array.fill(dimension)(core.bottom))

    /**
      * @inheritdoc
      */
    def top : Property = createProperty(Array.fill(dimension)(core.top))

    /**
      * @inheritdoc
      */
    def isBottom: Boolean = unreachable

    /**
      * @inheritdoc
      */
    def isEmpty: Boolean = unreachable

    /**
      * @inheritdoc
      */
    def isTop: Boolean = !unreachable && elements.forall( _.equals(core.top))

    /**
      * @inheritdoc
      */
    override def isPolyhedral: Boolean = false

    /**
      * @inheritdoc
      */
    override def nonDeterministicAssignment(n: Int): Property = {
      if (unreachable)
        return this
      createProperty(elements.updated(n, core.top))
    }

    /**
      * @inheritdoc
      */
    override def linearAssignment(pos: Int, lf: LinearForm): Property = {
      require(pos < elements.length && pos >= 0 && lf.dimension <= dimension)
      if (unreachable)
        return this
      val result : T = linearEvaluation(lf)
      createProperty(elements.updated(pos, result))
    }

    /**
      * @inheritdoc
      */
    override def addVariable(): Property =
      if (unreachable)
        BaseNumericalDomain.this.bottom(dimension + 1)
      else
        createProperty(elements :+ core.top, unreachable = false)

    /**
      * @inheritdoc
      */
    override def delVariable(pos: Int): Property = {
      require(pos < dimension && pos >= 0)
      val result = new Array[T](dimension - 1)
      // Copy the first pos-1 elements
      Array.copy(elements, 0, result, 0, pos)
      // Copy the remaining elements
      Array.copy(elements, pos + 1, result, pos, dimension - pos - 1)
      createProperty(result, unreachable)
    }

    /**
      * @inheritdoc
      */
    def mapVariables(rho: Seq[Int]): Property =  {
      require(rho.length == dimension)
      val newdim = rho.count(_ >= 0)
      require(rho forall { i => i >= -1 && i < newdim })
      // we do not check injectivity
      val result = new Array[T](newdim)
      for ((resultIndex, i) <- rho.zipWithIndex; if resultIndex >= 0)
        result(resultIndex) = elements(i)
      createProperty(result, unreachable)
    }

    /**
      * @inheritdoc
      */
    override def variableMul(n: Int, m: Int): Property = {
      elements(n) = core.mult(elements(n), elements(m))
      this
    }

    /**
      * @inheritdoc
      */
    override def variableDiv(n : Int, m : Int): Property = {
      elements(n) = core.division(elements(n), elements(m))
      this
    }

    /**
      * @inheritdoc
      */
    override def variableNeg(n: Int = dimension - 1): Property = {
      elements(n) = core.inverse(elements(n))
      this
    }

    def getElements : Array[T] = elements

    /**
      * @inheritdoc
      */
    def union(that: Property): Property =  {
      require(dimension == that.dimension)
      that match {
        case that : BaseProperty =>
          val result = (elements, that.elements).zipped.map( core.lub )
          createProperty(result, unreachable && that.unreachable)
        case _ => top
      }
    }

    /**
      * @inheritdoc
      */
    def intersection(that : Property): Property = {
      require(dimension == that.dimension)
      that match {
        case that : BaseProperty =>
          val result = (elements, that.elements).zipped.map( core.glb )
          createProperty(result, unreachable || that.unreachable)
        case _ => top
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
          val h = elements(i).toString
          s"${vars(i)} = $h"
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }


    /**
      * @inheritdoc
      */
    def minimize(lf: LinearForm): RationalExt =
      if (lf.homcoeffs.exists(!_.isZero))
        RationalExt.NegativeInfinity
      else
        RationalExt(lf.known)

    /**
      * @inheritdoc
      */
    def maximize(lf: LinearForm): RationalExt =
      if (lf.homcoeffs.exists(!_.isZero))
        RationalExt.PositiveInfinity
      else
        RationalExt(lf.known)

    /**
      * @inheritdoc
      */
    def frequency(lf: LinearForm): Option[Rational] =
      if (lf.homcoeffs.exists(!_.isZero))
        Option.empty
      else
        Option(lf.known)
    /**
      * Performs the comparison between two properties.
      * @param that the right hand side of the comparison
      * @param evidence$1 dk
      * @tparam B actual type of the right hand side
      * @return a Scala's tryCompareTo-like result
      */
    def tryCompareTo[B >: Property](that: B)(implicit evidence$1: (B) => PartiallyOrdered[B]): Option[Int] = that match {

      case that: BaseProperty =>
        require(dimension == that.dimension)
        (isEmpty, that.isEmpty) match {
          case (true, true) => Option(0)
          case (false, true) => Option(1)
          case (true, false) => Option(-1)
          case (false, false) =>
            val pairs = (elements, that.elements).zipped
            val comparisonList = pairs.map(core.compare)

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
      */
    protected def linearEvaluation(lf: LinearForm): T = {
      val known = lf.known.toDouble.toInt
      val homcoeffs = lf.homcoeffs.map(_.toDouble.toInt).toArray
      linearEvaluation(known, homcoeffs)
    }

    /**
      * @inheritdoc
      */
    protected def linearEvaluation(known: Int, homcoeffs: Array[Int]): T = {
      require(homcoeffs.length <= dimension)
      if (unreachable && homcoeffs.exists { _ != 0 })
        return core.top
      var acc: T = core.alpha(known)
      for (i <- homcoeffs.indices)
        if (homcoeffs(i) < 0)
          acc = core.sum(acc, core.inverse(elements(i)))
        else if(homcoeffs(i) > 0)
          acc = core.sum(acc, elements(i))

      acc
    }

    /**
      * @inheritdoc
      */
    override def widening(that: Property): Property = union(that)

    /**
      * @inheritdoc
      */
    // narrowing to be sound
    override def narrowing(that: Property): Property = this


    override type Domain = BaseNumericalDomain[T, CoreType]
    /**
      * Returns the abstract domain corresponding to this property.
      */
    override def domain: Domain = BaseNumericalDomain.this






}
}
