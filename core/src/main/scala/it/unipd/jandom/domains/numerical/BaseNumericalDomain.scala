/**
 * Copyright 2017 Mirko Bez, Stefano Munari, Sebastiano Valle
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You shosuld have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.{LinearForm, NumericalDomain, NumericalProperty}
import it.unich.jandom.utils.numberext.RationalExt
import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import spire.ClassTag
import spire.math.Rational
import scala.math.PartiallyOrdered

/**
  * Base abstract class for numerical domains. It contains the default implementation
  * of most of methods inherited from NumericalDomain. The implementations 
  * refers to Non-Relation Abstract Numerical Domains.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
abstract class BaseNumericalDomain
  [T : ClassTag, CoreType <: CompleteLatticeOperator[T] with IntOperator[T] with Abstraction[Int, T]](core : CoreType)
  extends NumericalDomain {

  /**
    * Handy factory method for properties, callable from the outside of the class.
    * @param elements array filled with domain elements
    * @param unreachable tells whether the program point is reachable or not
    * @return a new property for the given array
    */
    
  def createProperty(elements: Array[T], unreachable: Boolean) : Property


  def createProperty(p : BaseProperty): Property =
    createProperty(p.elements, p.isEmpty || p.elements.contains(core.bottom))

  //def updateData(x: Double, y: Double) = core.updateData(x,y)

  /**
    * Factory method for creating a Property.
    * @param elements of the domain
    * @return a new property
    */
  def createProperty(elements: Array[T]) : Property =
    createProperty(elements, elements.contains(core.bottom))
  //It is enough that one of the variable is bottom that the whole property is bottom

  /**
    * @inheritdoc
    */
  def bottom(n: Int): Property = createProperty(Array.fill(n)(core.bottom))

  /**
    * @inheritdoc
    */
  def top(n: Int): Property = createProperty(Array.fill(n)(core.top))

  /**
    * @inheritdoc
    */
    val widenings = Seq(WideningDescription.default[Property])

  /**
    * Property type that implements some common operations for numerical domains' properties.
    */
  abstract class BaseProperty (val elements: Array[T], val unreachable: Boolean) extends NumericalProperty[Property] {

    self: Property =>

    /**
      * @inheritdoc
      */
    override def dimension: Int = elements.length

    /**
      * @inheritdoc
      */
    override def constraints: Seq[LinearForm] = List()

    /**
      * @inheritdoc
      */
    override def bottom : Property = createProperty(Array.fill(dimension)(core.bottom))

    /**
      * @inheritdoc
      */
    override def top : Property = createProperty(Array.fill(dimension)(core.top))

    /**
      * @inheritdoc
      */
    override def isBottom: Boolean = unreachable

    /**
      * @inheritdoc
      */
    override def isEmpty: Boolean = unreachable

    /**
      * @inheritdoc
      */
    override def isTop: Boolean = !unreachable && elements.forall( _.equals(core.top))

    /**
      * @inheritdoc
      */
    override def isPolyhedral: Boolean = false

    /**
      * @inheritdoc
      */
    // forget function
    override def nonDeterministicAssignment(n: Int): Property = {
      if (isBottom)
        bottom
      else
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
    override def mapVariables(rho: Seq[Int]): Property =  {
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
    override def variableMul(n: Int, m: Int): Property =
      createProperty(elements.updated(n, core.mult(elements(n), elements(m))), unreachable)


    /**
      * @inheritdoc
      */
    override def variableDiv(n : Int, m : Int): Property = {
      createProperty(elements.updated(n, core.division(elements(n), elements(m))), unreachable)
    }

    /**
      * @inheritdoc
      */
    override def variableNeg(n: Int = dimension - 1): Property =
      createProperty(elements.updated(n, core.inverse(elements(n))), unreachable)


    /**
      * @inheritdoc
      */
    override def variableRem(n: Int, m : Int): Property = {
      createProperty(elements.updated(n, core.remainder(elements(n), elements(m))), unreachable)
    }

    /**
      * @inheritdoc
      */
    override def union(that: Property): Property =  {
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
    override def intersection(that : Property): Property = {
      require(dimension == that.dimension)
      that match {
        case that : BaseProperty =>
          val result = (elements, that.elements).zipped.map( core.glb )
          createProperty(result, unreachable || that.unreachable || result.contains(core.bottom))
        case _ =>
          top
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
          s"${vars(i)} $h"
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }

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
      * Performs the comparison between two properties.
      * @param that the right hand side of the comparison
      * @param evidence$1 implicit parameter which istantiates B to PartiallyOrdered
      * @tparam B actual type of the right hand side
      * @return 1 if `x` > `y` -- 0 if `x` = `y` -- -1 if `x` < `y`
      */
    override def tryCompareTo[B >: Property](that: B)(implicit evidence$1: (B) => PartiallyOrdered[B]): Option[Int] = that match {

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
      * Converts coefficients to integer and computes the linear evaluation of the
      * linear form
      *
      * @param lf a linear form
      * @return  the result (istantiated to the correct type T) of the linear evaluation of `lf`
      */
    protected def linearEvaluation(lf: LinearForm): T = {
      val known = lf.known.toInt
      val homcoeffs = lf.homcoeffs.map(_.toInt).toArray
      linearEvaluation(known, homcoeffs)
    }

    /**
      * Compute the sum of the coefficients of the linear form (absolute values)
      * if the CFG point is reachable, top if unreachable and not-constant lf (to keep soundness)
      *
      * @param known the known term of a linear form
      * @param homcoeffs homogeneous coefficients of a linear form
      * @return the result (istantiated to the correct type T) of the linear evaluation of a linear form
      */
    protected def linearEvaluation(known: Int, homcoeffs: Array[Int]): T = {
      require(homcoeffs.length <= dimension)
      if (unreachable && homcoeffs.exists { _ != 0 })
        return core.top
      var acc: T = core.alpha(known)
      for (i <- homcoeffs.indices) {
        if (homcoeffs(i) != 0) {
          val t = core.mult(core.alpha(homcoeffs(i)), elements(i))
          acc = core.sum(acc, t)
        }
      }
      acc
    }

    /**
      * @inheritdoc
      */
    override def widening(that: Property): Property = union(that)

    /**
      * @inheritdoc
      */
    override def narrowing(that: Property): Property = intersection(that)

    /**
      * Type used by superclass.
      */
    override type Domain = BaseNumericalDomain[T, CoreType]

    /**
      * @return the abstract domain corresponding to this property
      */
    override def domain: Domain = BaseNumericalDomain.this

  }
}
