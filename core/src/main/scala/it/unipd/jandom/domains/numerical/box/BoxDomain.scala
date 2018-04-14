/**
 * Copyright 2018 Mattia Bottaro, Mauro Carlin
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
package it.unipd.jandom.domains.numerical.box

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.BaseNumericalDomain
import it.unipd.jandom.domains.numerical.box.Box._
import it.unipd.jandom.domains.numerical.box.BoxDomainCore._
import it.unipd.jandom.domains.{InfInt, PositiveInfinity, NegativeInfinity, IntNumber}

/**
  * This is the domain of Integer boxes, also known as the interval domain. Bounds are represented by two numbers which type is :InfInt.
  * InfInt is a particular ad-hoc type that manages normal Int operation better than :Int type. For Example there's no overflow error, but it's
  * replaced by an "infinity" entity just like happens in :Double type.
  *
  * @author Mattia Bottaro <mattia.bottaro@studenti.unipd.it>
  * @author Mauro Carlin <mauro.carlin@studenti.unipd.it>
  */

class BoxDomain extends BaseNumericalDomain[Box, BoxDomainCore](BoxDomainCore()) {

  /**
    * @inheritdoc
    */
  override def createProperty(boxes: Array[Box], unreachable: Boolean): Property =
    new Property(boxes, unreachable)

  /**
    * Numerical property that tells whether the variables in a certain point of the CFG are an integer interval or not.
    * @param boxes array of the variables' boxes status
    * @param unreachable tells if a given program point is unreachable
    */
  class Property (boxes : Array[Box], unreachable: Boolean) extends BaseProperty(boxes, unreachable) {

    /**
      * @param x is an Interval
      * @return the lower bound.
      */
    private def projectLow (box : Box) : InfInt = {

      box match {
        case IntervalBottom => PositiveInfinity()
        case IntervalTop => NegativeInfinity()
        case Interval(low,high) => low
      }
    }


    /**
      * @param x is an Interval
      * @return the upper bound.
      */
    private def projectHigh (box : Box) : InfInt = {

      box match {
        case IntervalBottom => NegativeInfinity()
        case IntervalTop => PositiveInfinity()
        case Interval(low,high) => high
      }
    }

	  def apply(boxes: Array[Box], unreachable: Boolean) : Property = new Property(boxes, unreachable)
    // x != 0
    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm): Property = {
      if (isEmpty)
        return this
      val result : Box = linearEvaluation(lf);
      result match {
        case IntervalBottom => bottom
        case Interval(low,high) => if (low == high && low == IntNumber(0)) bottom else this
        case _ => this
      }
    }

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm): Property = {
      if (isEmpty)
        return this
      val homcoeffs = lf.homcoeffs.map(_.toInt).toArray
      val known = IntNumber(lf.known.toInt)
      val lfMin = projectLow(linearEvaluation(lf))
      val lfArgmin = linearArgmin(lf);
      //// print(lowresult)
      if (lfMin > IntNumber(0))
        return bottom
      else {
        var newboxes = boxes.clone
        val infinities = (homcoeffs.indices) filter { i => lfArgmin(i).isInfinity && homcoeffs(i) != 0 }

        infinities.size match {
          case 0 => {
            // print("CASE 0")
            for (i <- homcoeffs.indices) {
              if (homcoeffs(i) < 0) newboxes(i) = Interval(projectLow(boxes(i)) max (lfArgmin(i) - (lfMin / IntNumber(homcoeffs(i)))), projectHigh(newboxes(i)))
              if (homcoeffs(i) > 0) newboxes(i) = Interval(projectLow(newboxes(i)), projectHigh(boxes(i)) min (lfArgmin(i) - (lfMin / IntNumber(homcoeffs(i)))))
            }
          }
          case 1 => {
            // print("CASE 1")
            val posinf = infinities.head
            if (homcoeffs(posinf) < 0) {
              // print("MINORE")
              // print("LOW = " + projectLow(boxes(posinf)))
              // print("ALTRO = " + ((dotprod(homcoeffs, lfArgmin, posinf).inverse() - known) / IntNumber(homcoeffs(posinf))))
              // print("LFARGMIN = "+lfArgmin)
              newboxes(posinf) = Interval(projectLow(boxes(posinf)) max ((dotprod(homcoeffs, lfArgmin, posinf).inverse() - known) / IntNumber(homcoeffs(posinf))), projectHigh(newboxes(posinf)))
            } else {
              // print("MAGGIORE")
              newboxes(posinf) = Interval(projectLow(boxes(posinf)), projectHigh(boxes(posinf)) min ((dotprod(homcoeffs, lfArgmin, posinf).inverse() - known) / IntNumber(homcoeffs(posinf))))
            }
          }
          case _ =>
        }
        new Property(newboxes,false)
      }
    }

    private def linearArgmin(lf: LinearForm): Seq[InfInt] = {
      (lf.homcoeffs.zipWithIndex) map {
        case (c, i) => if (c > 0) projectLow(boxes(i)) else projectHigh(boxes(i))
      }
    }

    private def dotprod(x: Seq[Int], y: Seq[InfInt], remove: Int): InfInt = {
      var sum: InfInt = IntNumber(0)
      for (i <- x.indices; if i != remove && x(i) != 0)
        sum = sum + (IntNumber(x(i)) * y(i))
      sum
    }

    /**
      * Implementing the widening strategy described in
      * https://hal.archives-ouvertes.fr/hal-01657536 (Tutorial on Static Inference of Numeric Invariants by Abstract Interpretation), page 221
      *
      * @param that the abstract object to be widened with `this`. `that` IS assumed to be smaller than `this`.
      * @return the widening of the two abstract properties.
      */
    override def widening(that : Property) : Property = {
      createProperty((this.elements, that.elements).zipped.map((x, y) => {
        (x,y) match {
          case (IntervalBottom, _) => y
          case (_ , IntervalBottom) => x
          case (IntervalTop, _) => IntervalTop
          case (_ , IntervalTop) => IntervalTop
          case (Interval(low1, high1), Interval(low2,high2)) =>
            // TODO
            var newhigh = high1
            var newlow = low1

            if (low2 >= low1)
              newlow = low1
            else
              newlow = NegativeInfinity()

            if (high1 >= high2)
              newhigh = high1
            else
              newhigh = PositiveInfinity()

            Interval(newlow,newhigh)
        }
      }), this.isEmpty && that.isEmpty)
	  }

    /**
      * Implementing the narrowing strategy
      *
      * @param that the abstract object to be narrowed with `this`. `that` IS assumed to be smaller than `this`.
      * @return the narrowing of the two abstract properties.
      */
	  override def narrowing(that : Property) : Property = {
      createProperty((this.elements, that.elements).zipped.map((x, y) => {
        (x,y) match {
          case (IntervalBottom, _) => IntervalBottom
          case (_ , IntervalBottom) => x
          case (IntervalTop, _) => y
          case (_ , IntervalTop) => x
          case (Interval(low1, high1), Interval(low2,high2)) =>
            // TODO
            var newlow = low1
            var newhigh = high1
            if (low1 == NegativeInfinity()) newlow = low2
            if (high1 == PositiveInfinity()) newhigh = high2
            Interval(newlow,newhigh)
        }
      }), this.isEmpty && that.isEmpty)
	  }

  } // end of Property
} // end of BoxDomain (class)

object BoxDomain {
  /**
    * Factory method of BoxDomain
    */
  def apply() = new BoxDomain()
}
