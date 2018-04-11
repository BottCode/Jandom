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
import it.unipd.jandom.domains.numerical.box.BoundedBoxDomainCore._
import it.unipd.jandom.domains.{InfInt, PositiveInfinity, NegativeInfinity, IntNumber}

/**
  *
  *
  * @author Mattia Bottaro <mattia.bottaro@studenti.unipd.it>
  * @author Mauro Carlin <mauro.carlin@studenti.unipd.it>
  */
class BoundedBoxDomain(m : InfInt, n : InfInt) extends BaseNumericalDomain[Box, BoundedBoxDomainCore](BoundedBoxDomainCore(m,n)){

  override def updateData(x: Double, y: Double): Unit = {
    val new_m = IntNumber(x.toInt)
    val new_n = IntNumber(y.toInt)
    BoundedBoxDomainCore.updateData(new_m,new_n)
  }

  /**
    * @inheritdoc
    */
  override def createProperty(boxes: Array[Box], unreachable: Boolean): Property = {
    //boxes.map(normalizeBound)
    new Property(boxes, unreachable)
  }

  /**
    * Numerical property that tells whether the variables in a certain point of the CFG are constant or not.
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
      //print(lowresult)
      if (lfMin > IntNumber(0))
        return bottom
      else {
        var newboxes = boxes.clone
        val infinities = (homcoeffs.indices) filter { i => lfArgmin(i).isInfinity && homcoeffs(i) != 0 }

        infinities.size match {
          case 0 => {
            for (i <- homcoeffs.indices) {
              if (homcoeffs(i) < 0) newboxes(i) = Interval(projectLow(boxes(i)) max (lfArgmin(i) - (lfMin / IntNumber(homcoeffs(i)))), projectHigh(newboxes(i)))
              if (homcoeffs(i) > 0) newboxes(i) = Interval(projectLow(newboxes(i)), projectHigh(boxes(i)) min (lfArgmin(i) - (lfMin / IntNumber(homcoeffs(i)))))
            }
          }
          case 1 => {
            val posinf = infinities.head
            if (homcoeffs(posinf) < 0)
                newboxes(posinf) = Interval(projectLow(boxes(posinf)) max ((dotprod(homcoeffs, lfArgmin, posinf).inverse() - known) / IntNumber(homcoeffs(posinf))), projectHigh(newboxes(posinf)))
              else
                newboxes(posinf) = Interval(projectLow(boxes(posinf)), projectHigh(boxes(posinf)) min ((dotprod(homcoeffs, lfArgmin, posinf).inverse() - known) / IntNumber(homcoeffs(posinf))))
          }
          case _ =>
        }
        new Property(newboxes.map(BoundedBoxDomainCore.norm),false)
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

  } // End of property

} // End of BoundedBoxDomain

object BoundedBoxDomain {
  /**
    * Factory method of BoxDomain
    */
  def apply(m : InfInt, n : InfInt) = new BoundedBoxDomain(m,n)
}
