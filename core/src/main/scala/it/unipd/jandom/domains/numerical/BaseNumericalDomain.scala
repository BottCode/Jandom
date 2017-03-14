package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.{LinearForm, NumericalDomain, NumericalProperty}
import it.unich.jandom.utils.numberext.RationalExt
import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import spire.math.Rational

import scala.math.PartiallyOrdered

/**
  * @note DomainType is the trait of the XDomainCore object (e.g. Sign, Constant, etc.)
  */
class BaseNumericalDomain
  [T, DomainType <: CompleteLatticeOperator[T] with IntOperator[T] with Abstraction[Int, T]](domain : DomainType)
  extends NumericalDomain {

  type Property = BaseProperty

  def apply(elements: Array[T]): BaseProperty = BaseProperty(elements, elements.forall(p => p.equals(domain.bottom)))

  /**
    * @inheritdoc
    */
  def bottom(n: Int) = BaseProperty(Array.fill(n)(domain.bottom), true)

  /**
    * @inheritdoc
    */
  def top(n: Int) = BaseProperty(Array.fill(n)(domain.top), false)

  /**
    * @inheritdoc
    */
    val widenings = Seq(WideningDescription.default[BaseProperty])

  /**
    * @inheritdoc
    * For numerical domains, properties needs to be instances of
    */
  abstract case class BaseProperty private[BaseNumericalDomain](elements: Array[T], unreachable: Boolean)
    extends NumericalProperty[BaseProperty] {

    type Domain = DomainType

    def apply(elements: Array[T], unreachable: Boolean): BaseProperty

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
    def bottom : BaseProperty = BaseProperty(Array.fill(dimension)(domain.bottom), true)

    /**
      * @inheritdoc
      */
    def top : BaseProperty = BaseProperty(Array.fill(dimension)(domain.top), false)

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
    def isTop: Boolean = !unreachable && elements.forall( _.equals(domain.top))

    /**
      * @inheritdoc
      */
    override def isPolyhedral: Boolean = false

    /**
      * @inheritdoc
      */
    override def nonDeterministicAssignment(n: Int): BaseProperty = {
      if (unreachable)
        return this
      BaseProperty(elements.updated(n, domain.top), false)
    }

    /**
      * @inheritdoc
      */
    override def linearAssignment(pos: Int, lf: LinearForm): BaseProperty = {
      require(pos < elements.length && pos >= 0 && lf.dimension <= dimension)
      if (unreachable)
        return this
      val result : T = linearEvaluation(lf)
      BaseProperty(elements.updated(pos, result), false)
    }

    /**
      * @inheritdoc
      */
    override def addVariable(): BaseProperty =
      if (unreachable)
        BaseNumericalDomain.this.bottom(dimension + 1)
      else
        BaseNumericalDomain.this(elements :+ domain.top)

    /**
      * @inheritdoc
      */
    override def delVariable(pos: Int): BaseProperty = {
      require(pos < dimension && pos >= 0)
      val result = new Array[T](dimension - 1)
      // Copy the first pos-1 elements
      Array.copy(elements, 0, result, 0, pos)
      // Copy the remaining elements
      Array.copy(elements, pos + 1, result, pos, dimension - pos - 1)
      BaseProperty(result, unreachable)
    }

    /**
      * @inheritdoc
      */
    def mapVariables(rho: Seq[Int]): BaseProperty =  {
      require(rho.length == dimension)
      val newdim = rho.count(_ >= 0)
      require(rho forall { i => i >= -1 && i < newdim })
      // we do not check injectivity
      val result = new Array[T](newdim)
      for ((resultIndex, i) <- rho.zipWithIndex; if resultIndex >= 0)
        result(resultIndex) = elements(i)
      BaseProperty(result, unreachable)
    }

    /**
      * @inheritdoc
      */
    override def variableMul(n: Int, m: Int): BaseProperty = {
      elements(n) = domain.mult(elements(n), elements(m))
      this
    }

    /**
      * @inheritdoc
      */
    override def variableDiv(n : Int, m : Int): BaseProperty = {
      elements(n) = domain.division(elements(n), elements(m))
      this
    }

    /**
      * @inheritdoc
      */
    override def variableNeg(n: Int = dimension - 1): BaseProperty = {
      elements(n) = domain.inverse(elements(n))
      this
    }

    /**
      * @inheritdoc
      */
    def union(that: BaseProperty): BaseProperty =  {
      require(dimension == that.dimension)
      val result = (this.elements, that.elements).zipped.map( domain.lub )
      BaseProperty(result, unreachable && that.unreachable)
    }

    /**
      * @inheritdoc
      */
    def intersection(that: BaseProperty): BaseProperty = {
      require(dimension == that.dimension)
      val result = (this.elements, that.elements).zipped.map( domain.glb )
      BaseNumericalDomain.this(result)
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

    def tryCompareTo[B >: BaseProperty](that: B)(implicit evidence$1: (B) => PartiallyOrdered[B]): Option[Int] = that match {

      case that: BaseProperty =>
        require(dimension == that.dimension)
        (isEmpty, that.isEmpty) match {
          case (true, true) => Option(0)
          case (false, true) => Option(1)
          case (true, false) => Option(-1)
          case (false, false) =>
            val pairs = (this.elements, that.elements).zipped
            val comparisonList = pairs.map(domain.compare)

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
    private def linearEvaluation(lf: LinearForm): T = {
      val known = lf.known.toDouble.toInt
      val homcoeffs = lf.homcoeffs.map(_.toDouble.toInt).toArray
      linearEvaluation(known, homcoeffs)
    }

    /**
      * @inheritdoc
      */
    private def linearEvaluation(known: Int, homcoeffs: Array[Int]): T = {
      require(homcoeffs.length <= dimension)
      if (unreachable && homcoeffs.exists { _ != 0 })
        return domain.top
      var acc: T = domain.alpha(known)
      for (i <- homcoeffs.indices)
        if (homcoeffs(i) < 0)
          acc = domain.sum(acc, domain.inverse(elements(i)))
        else if(homcoeffs(i) > 0)
          acc = domain.sum(acc, elements(i))

      acc
    }

    /**
      * @inheritdoc
      */
    override def widening(that: BaseProperty): BaseProperty = union(that)

    /**
      * @inheritdoc
      */
    // narrowing to be sound
    override def narrowing(that: BaseProperty): BaseProperty = this

    /**
      * Returns the abstract domain corresponding to this property.
      */
    override def domain: DomainType = domain
  }
}