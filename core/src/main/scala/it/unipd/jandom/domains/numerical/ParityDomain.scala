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
import ParityDomainCore._
import spire.math.Rational

import scala.math.PartiallyOrdered

object ParityDomain extends NumericalDomain {

  def apply(parities: Array[Parity]): Property = Property(parities, parities.forall(p => p.equals(ParityBottom)))

  case class Property private[ParityDomain](parity : Array[Parity], unreachable: Boolean) extends NumericalProperty[Property] {

    override type Domain = ParityDomain.type

    /**
      * @inheritdoc
      */
    override def nonDeterministicAssignment(n: Int): Property = {
      if (unreachable)
        return this
      Property(parity.updated(n, ParityTop), unreachable = false)
    }


    /**
      * @inheritdoc
      */
    private def linearEvaluation(lf: LinearForm): Parity = {
      val known = lf.known.toDouble
      val homcoeffs = lf.homcoeffs.map(_.toDouble).toArray
      linearEvaluation(known, homcoeffs)
    }

    /**
      * @inheritdoc
      */
    private def linearEvaluation(known: Double, homcoeffs: Array[Double]): Parity = {
      require(homcoeffs.length <= dimension)
      if (unreachable && homcoeffs.exists { _ != 0 }) return ParityTop
      var p: Parity = toParity(known)
      for (i <- homcoeffs.indices)
        if (homcoeffs(i) != 0)
          p = sum(p, parity(i))
      p
    }

    /**
      * @inheritdoc
      */
    override def linearAssignment(pos: Int, lf: LinearForm): Property = {
      require(pos < parity.length && pos >= 0 && lf.dimension <= dimension)
      if (unreachable)
        return this
      val p: Parity = linearEvaluation(lf)
      Property(parity.updated(pos, p), unreachable = false)
    }

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm): Property = {
      if (unreachable)
        return this
      val p: Parity = linearEvaluation(lf)
      p match {
        case Even => this
        case Odd => this
        case ParityTop => top
        case ParityBottom => bottom
      }
    }

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm): Property = linearDisequality(lf)

    /**
      * @inheritdoc
      */
    override def isPolyhedral: Boolean = false

    /**
      * @inheritdoc
      */
    override def constraints: Seq[LinearForm] = List()

    /**
      * @inheritdoc
      */
    override def addVariable(): Property =
      if (unreachable)
        ParityDomain.this.bottom(dimension + 1)
      else
        ParityDomain.this(parity :+ ParityTop)

    /**
      * @inheritdoc
      */
    override def delVariable(pos: Int): Property = {
      require(pos < parity.length && pos >= 0)
      val newParity = new Array[Parity](parity.length - 1)
      // Copy the first pos-1 elements
      Array.copy(parity, 0, newParity, 0, pos)
      // Copy the remaining elements
      Array.copy(parity, pos + 1, newParity, pos, parity.length - pos - 1)
      Property(newParity, unreachable)
    }

    /**
      * @inheritdoc
      */
    def mapVariables(rho: Seq[Int]): Property =  {
      require(rho.length == dimension)
      // TODO: Check this method
      val newdim = rho.count(_ >= 0)
      require(rho forall { i => i >= -1 && i < newdim })
      // we do not check injectivity
      val newParity = new Array[Parity](newdim)
      for ((newi, i) <- rho.zipWithIndex; if newi >= 0)
        newParity(newi) = parity(i)
      Property(newParity, unreachable)
    }

    /**
      * @inheritdoc
      */
    def bottom: Property = ParityDomain.bottom(parity.length)

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
    def top: Property = ParityDomain.top(parity.length)

    /**
      * @inheritdoc
      */
    def isTop: Boolean = !unreachable && parity.forall( _.equals(ParityTop) )

    /**
      * @inheritdoc
      */
    def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (unreachable)
        "[ empty ]"
      else {
        val bounds = for (i <- 0 until dimension) yield {
          val h = parity(i) match {
            case Even => "EVEN"
            case Odd => "ODD"
            case ParityTop => "TOP"
            case ParityBottom => "BOTTOM"
          }
          s"${vars(i)} = $h"
        }
      bounds.mkString("[ ", " , ", " ]")
    }
  }

    /**
      * @inheritdoc
      */
    def dimension: Int = parity.length

    /**
      * @inheritdoc
      */
    def domain = ParityDomain

    /**
      * @inheritdoc
      */
    def widening(that: Property): Property =
      // TODO: Check this implementation
      Property((this.parity, that.parity).zipped.map(lub), this.isEmpty && that.isEmpty)

    /**
      * @inheritdoc
      */
    def narrowing(that: Property): Property =  {
      // TODO: Add implementation
      println("Narrowing called")
      println(s"This: $this")
      println(s"That: $that")
      println("Narrowing: " + this)
      this
    }

    /**
      * @inheritdoc
      */
    def union(that: Property): Property =  {
      require(dimension == that.dimension)
      val newParity = (this.parity, that.parity).zipped.map( lub )
      Property(newParity, unreachable && that.unreachable)
    }

    /**
      * @inheritdoc
      */
    def intersection(that: Property): Property = {
      require(dimension == that.dimension)
      val newParity = (this.parity, that.parity).zipped.map( glb )
      ParityDomain.this(newParity)
    }

    def tryCompareTo[B >: Property](that: B)(implicit evidence$1: (B) => PartiallyOrdered[B]): Option[Int] = that match {

      case that: Property =>
        require(dimension == that.dimension)
        (isEmpty, that.isEmpty) match {
          case (true, true) => Option(0)
          case (false, true) => Option(1)
          case (true, false) => Option(-1)
          case (false, false) =>
            val parityPairs = (this.parity, that.parity).zipped
            val comparisonList = parityPairs.map(compare)

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

  } // end of Property

  /**
    * @inheritdoc
    */
  def bottom(n: Int) = Property(Array.fill(n)(ParityBottom), unreachable = true)

  /**
    * @inheritdoc
    */
  def top(n: Int) = Property(Array.fill(n)(ParityTop), unreachable = false)

  /**
    * @inheritdoc
    */
  val widenings = Seq(WideningDescription.default[Property])

}
