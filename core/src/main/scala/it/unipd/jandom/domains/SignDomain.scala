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

package it.unipd.jandom.domains

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.{LinearForm, NumericalDomain, NumericalProperty}
import it.unich.jandom.utils.numberext.RationalExt
import spire.math.Rational


/**
  * Sign domain
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>, Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *           Stefano Munari <stefano.munari@studenti.unipd.it>
 */
object SignDomain extends NumericalDomain {

  import it.unipd.jandom.domains.SignDomainCore._

  case class Property private[SignDomain] (sign : Array[Sign], unreachable: Boolean) extends NumericalProperty[Property] {
    val dimension: Int = sign.length
    require(dimension >= 0)

    type Domain = SignDomain.type

    def domain = SignDomain



    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {

      case other: Property =>
        require(dimension == other.dimension)
        (isEmpty, other.isEmpty) match {
          case (true, true) => Option(0)
          case (false, true) => Option(1)
          case (true, false) => Option(-1)
          case (false, false) =>
            val signPairs = (this.sign, other.sign).zipped
            val comparisonList = signPairs.map(compare)

            if (comparisonList.forall {
              case Some(i) => if (i == 0) true else false
              case None => false
            })
              Option(0)

            else if (comparisonList.forall {
              case Some(i) => if (i == 1) true else false
              case None => false
            })
              Option(1)
            else if (comparisonList.forall {
              case Some(i) => if (i == -1) true else false
              case None => false
            })
              Option(-1)
            else
              Option.empty
        }

      case _ => None
    }

    //TODO: Add widening implementation
    def widening(that: Property): Property = {
      println("Widening called")
      println(s"This: $this ${this.isEmpty}")
      println(s"That: $that ${that.isEmpty}")
      println("Widening: " + this)
      Property((this.sign, that.sign).zipped.map(lub), this.isEmpty && that.isEmpty)

    }


    /* Compute an upper bound of two abstract properties. */
    def union(that: Property) : Property = {
      println("Union called")
      require(dimension == that.dimension)
      val newSign = (this.sign, that.sign).zipped.map( lub )
      println(s"This: $this")
      println(s"That: $that")
      println("Lub: " + Property(newSign, unreachable && that.unreachable))
      Property(newSign, unreachable && that.unreachable)
    }


    //TODO: Add narrowing implementation
    def narrowing(that: Property): Property = {
      println("Narrowing called")
      println(s"This: $this")
      println(s"That: $that")
      println("Narrowing: " + this)
      this
    }


    /**
      * @inheritdoc
      * @note @inheritdoc
      */
     /*Compute an upper approximation of the greatest lower bound of two abstract properties.*/
    def intersection(that: Property) : Property = {
      println("Intersection called")
      require(dimension == that.dimension)
      val newSign = (this.sign, that.sign).zipped.map( glb )
      /*println(s"This: $this")
      println(s"That: $that")
      println("Glb: " + SignDomain.this(newSign))*/
      SignDomain.this(newSign)
     }


    /**
      * @inheritdoc
      * @note @inheritdoc
      // * @throws IllegalArgumentException
      */
    def nonDeterministicAssignment(n: Int): Property = {
      if(unreachable)
        return this
      Property(sign.updated(n, SignTop), unreachable = false)
    }



    /**
      * Compute the minimum and maximum value of a linear form in a box.
      *
      * @param lf a linear form
      * @return a tuple with two components: the first component is the least value, the second component is the greatest value
      * of the linear form over the box.
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
      * @return a tuple with two components: the first component is the least value, the second component is the greatest value
      * of the linear form over the box.
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

    def linearAssignment(pos: Int, lf: LinearForm) : Property = {
      require(pos < sign.length && pos >= 0 && lf.dimension <= dimension)
      println(s"Assigning... $this \nwith linear form: $lf \nand number n: $pos")
      if(unreachable)
        return this
      val s : Sign = linearEvaluation(lf)
      println("Generated: " + Property(sign.updated(pos, s), unreachable = false))
      Property(sign.updated(pos, s), unreachable = false)
    }


    /** @inheritdoc
       * @param lf expression that gets evaluated for the linear inequality
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
      * @param lf expression that gets evaluated for the linear disequality
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
     * Add new variable maintaining the current values
     * @note @inheritdoc
     // * @throws IllegalArgumentException
     */
    def addVariable(): Property = {
      println(s"Adding variable at the $dimension position")
      if (unreachable)
        return SignDomain.this.bottom(dimension + 1)
      SignDomain.this(sign :+ SignTop)
    }

    /**
     * @inheritdoc
     * This is a complete operator for boxes.
     * @note @inheritdoc
     // * @throws IllegalArgumentException
     */
    def delVariable(pos: Int): Property = {
      require(pos < sign.length && pos >= 0)
      println(s"Deleting variable at $pos position")
      println(s"This: $this")
      /*if(sign.init.forall(s => s.equals(SignBottom)))
        return new Property(Array.fill[Sign](sign.length - 1)(SignTop), false)*/
      val newSign = new Array[Sign](sign.length - 1)
      // Copy the first pos-1 elements
      Array.copy(sign, 0, newSign, 0, pos)
      // Copy the remaining elements
      Array.copy(sign, pos + 1, newSign, pos, sign.length - pos - 1)
      Property(newSign, unreachable)
    }


    /* Done in complete analogy to the BoxDoubleDomain */
    def mapVariables(rho: Seq[Int]): Property = {
      require(rho.length == dimension)
      val newdim = rho.count(_ >= 0)
      require(rho forall { i => i >= -1 && i < newdim })
      // we do not check injectivity
      val newSign = new Array[Sign](newdim)

      for ((newi, i) <- rho.zipWithIndex; if newi >= 0) {
        newSign(newi) = sign(i)
      }
      Property(newSign, unreachable)
    }

    def isEmpty: Boolean = unreachable

    def isTop: Boolean = !isEmpty && sign.forall(s => s.equals(SignTop))
    def isBottom: Boolean = isEmpty

    def bottom: Property = SignDomain.bottom(sign.length)
    def top: Property = SignDomain.top(sign.length)

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
            case SignTop => "TOP"
            case SignBottom => "BOTTOM"
            case Plus => "POSITIVE"
            case Minus => "NEGATIVE"
            case Zero => "ZERO"
          }
          s"${vars(i)} = $h"
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }



    /**
      * Assignments of the kind `vn = vn * vm`.  The standard implementation determines
      * whether `vn` or `vm` is a constant, and use linearAssignment in such a case.
      * Otherwise, it resorts to a non deterministic assignment.
      *
      * @note $NOTEN
      */
    override def variableMul(n: Int, m: Int): Property = {
      println("Multiplication")
      sign(n) = mult(sign(n), sign(m))
      this
    }

    override def variableDiv(n : Int, m : Int): Property = {
      println("Division")
      sign(n) = division(sign(n), sign(m))
      this
    }

     /**
       * @inheritdoc
       * @note @inheritdoc
      */
    override def variableNeg(n: Int = dimension - 1): Property = {
      sign(n) = inverse(sign(n))
      this
    }


    /**
      * @inheritdoc
      * @note @inheritdoc
      */
    override def variableRem(n: Int = dimension - 2, m: Int = dimension - 1): Property = {
      sign(n) = remainder(sign(n), sign(m))
      this
    }



  }

  def apply(signsArray : Array[Sign]): Property = {
    Property(signsArray, signsArray.forall(s => s.equals(SignBottom)))
  }


  val widenings = Seq(WideningDescription.default[Property])



  def top(n: Int) = Property(Array.fill(n)(SignTop), unreachable = false)
  def bottom(n: Int) = Property(Array.fill(n)(SignBottom), unreachable = true)
}
