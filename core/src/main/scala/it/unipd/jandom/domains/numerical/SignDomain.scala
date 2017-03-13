/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.{LinearForm, NumericalDomain, NumericalProperty}
import it.unich.jandom.utils.numberext.RationalExt
import spire.math.Rational


/**
  * Sign domain, i.e. the domain composed of the elements Minus (negative numbers), Plus (positive numbers) and
  * Zero (0). SignTop and SignBottom complete the lattice, providing a greatest and a least element for this set.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>, Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *           Stefano Munari <stefano.munari@studenti.unipd.it>
 */
class SignDomain extends NumericalDomain {

  // this class uses the operations defined in SignDomainCore
  import SignDomainCore._

  /**
    * Numerical property that describes the sign of the variables in a certain point of the CFG.
    * @param sign array of the variables' signs
    * @param unreachable tells if a given program point is unreachable
    */
  case class Property private[SignDomain](sign : Array[Sign], unreachable: Boolean) extends NumericalProperty[Property] {
    val dimension: Int = sign.length   // pretty straightforward
    require(dimension >= 0)

    type Domain = SignDomain

    def domain : NumericalDomain = SignDomain.this

    // returns an array of all-top variables
    def top: Property = Property(Array.fill(sign.length)(SignTop), unreachable = false)
    // returns an array of all-bottom variables
    def bottom : Property = Property(Array.fill(sign.length)(SignBottom), unreachable = true)

    /**
      * Performs the comparison between two properties.
      * @param other the right hand side of the comparison
      * @param arg0 dk
      * @tparam B actual type of the right hand side
      * @return a Scala's tryCompareTo-like result
      */
    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {

      case other: Property =>
        require(dimension == other.dimension)
        (isEmpty, other.isEmpty) match {
          case (true, true) => Option(0)
          case (false, true) => Option(1)   // _ > unreachable
          case (true, false) => Option(-1)  // _ < unreachable
          case (false, false) =>            // non-trivial case
            val signPairs = (this.sign, other.sign).zipped
            val comparisonList = signPairs.map(compare)

            if (comparisonList.forall {
              case Some(i) => if (i == 0) true else false
              case None => false
            })
              Option(0)
            else if (comparisonList.forall { // lhs >= rhs
              case Some(i) => if (i >= 0) true else false
              case None => false
            } && comparisonList.exists {     // requires strict > for at least one element
              case Some(i) => if (i == 1) true else false
              case None => false
            })
              Option(1)
            else if (comparisonList.forall { // lhs <= rhs
              case Some(i) => if (i <= 0) true else false
              case None => false
            } && comparisonList.exists {     // requires strict < for at least one element
              case Some(i) => if (i == -1) true else false
              case None => false
            })
              Option(-1)
            else                             // non-comparable arrays
              Option.empty
        }
      // comparison not defined for other properties' types
      case _ => None
    }

    /**
      * @inheritdoc
      */
    def union(that: Property) : Property = {
      require(dimension == that.dimension)
      Property((this.sign, that.sign).zipped.map( lub ), unreachable && that.unreachable)
    }

    /**
      * @inheritdoc
      */
    def intersection(that: Property) : Property = {
      require(dimension == that.dimension)
      SignDomain.this((this.sign, that.sign).zipped.map( glb ))
    }

    /**
      * @inheritdoc
      */
    def widening(that: Property): Property = union(that)

    /**
      * @inheritdoc
      */
    // it is likely that it can't be improved, but returning `this` preserves the soundness
    def narrowing(that: Property): Property = this


    /**
      * @inheritdoc
      */
    def nonDeterministicAssignment(n: Int): Property =
      if(unreachable)
        return this
      else
        Property(sign.updated(n, SignTop), unreachable = false)

    /**
      * Compute the minimum and maximum value of a linear form in a box.
      *
      * @param lf a linear form
      * @return  the sign of the linear evaluation of `lf`
      */
    private def linearEvaluation(lf: LinearForm): Sign = {
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
    private def linearEvaluation(known: Double, homcoeffs: Array[Double]): Sign = {
      require(homcoeffs.length <= dimension)
      var s: Sign = toSign(known)
      if (unreachable && homcoeffs.exists { _ != 0 }) return SignTop
      for (i <- homcoeffs.indices) {
        if (homcoeffs(i) > 0) {
          val t: Sign = sign(i)
          s = sum(s, t)
        }
        else if (homcoeffs(i) < 0) {
          val t: Sign = inverse(sign(i))
          s = sum(s, t)
        }
      }
      s
    }

    /**
      * @inheritdoc
      */
    def linearAssignment(pos: Int, lf: LinearForm) : Property = {
      require(pos < sign.length && pos >= 0 && lf.dimension <= dimension)
      if(unreachable)
        return this
      val s : Sign = linearEvaluation(lf)
      Property(sign.updated(pos, s), unreachable = false)
    }


    /** @inheritdoc
      */
    def linearInequality(lf: LinearForm) : Property = {
      val s : Sign = linearEvaluation(lf)
      if (isEmpty)
        return this
      s match {
        case Plus => bottom
        case SignTop => top //IS this the best correct approximation???? //TODO Maybe we can improve this result Case Top????
        case SignBottom => bottom //TODO Check it
        case _ => this
      }
    }

    /**
      * @inheritdoc
      */
    def linearDisequality(lf: LinearForm) : Property = {
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
      * @inheritdoc
      */
    def constraints = List()

    /**
      * @inheritdoc
      */
    def isPolyhedral = false

    /**
     * @inheritdoc
     */
    def addVariable(): Property =
      if (unreachable)
        return SignDomain.this.bottom(dimension + 1)
      else
        SignDomain.this(sign :+ SignTop)

    /**
     * @inheritdoc
     */
    def delVariable(pos: Int): Property = {
      require(pos < sign.length && pos >= 0)
      val newSign = new Array[Sign](sign.length - 1)
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
      val newSign = new Array[Sign](newdim)
      for ((newi, i) <- rho.zipWithIndex; if newi >= 0)
        newSign(newi) = sign(i)
      Property(newSign, unreachable)
    }

    /**
      * @inheritdoc
      */
    def isEmpty: Boolean = unreachable

    /**
      * @inheritdoc
      */
    def isTop: Boolean = !isEmpty && sign.forall( _.equals(SignTop))

    /**
      * @inheritdoc
      */
    def isBottom: Boolean = isEmpty

    /**
      * @inheritdoc
      */
    def top(n: Int) = Property(Array.fill(n)(SignTop), unreachable = false)

    /**
      * @inheritdoc
      */
    def bottom(n: Int) = Property(Array.fill(n)(SignBottom), unreachable = true)

     /**
     * @inheritdoc
     */
    def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (unreachable)
        "empty"
      else {
        val bounds = for (i <- 0 until dimension) yield {
          val h = sign(i) match {
            case SignTop => "TOP"
            case SignBottom => "BOTTOM"
            case Plus => "PLUS"
            case Minus => "Minus"
            case Zero => "Zero"
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



  } // end of SignDomain's Property

  /**
    * Handy ctor for properties, callable from the outside of the class
    * @param signsArray array filled with elements in SignDomain
    * @return a new property for `signsArray`
    */
  def apply(signsArray : Array[Sign]): Property = {
    Property(signsArray, signsArray.forall( _.equals(SignBottom)) )
  }

  /**
    * Specifies which widenings are available for SignDomain.
    */
  val widenings = Seq(WideningDescription.default[Property])

  /**
    * @inheritdoc
    */
  def top(n: Int) = Property(Array.fill(n)(SignTop), unreachable = false)

  /**
    * @inheritdoc
    */
  def bottom(n: Int) = Property(Array.fill(n)(SignBottom), unreachable = true)
} // end of SignDomain class

object SignDomain {
  /**
    * Returns an abstract domain for boxes which is correct w.r.t. real arithmetic or
    * double arithmetic, according to the parameter `overReals`.
    */
  def apply() = new SignDomain()
} // end of SignDomain's companion object
