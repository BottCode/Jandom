/**
 * Copyright 2017 Mattia Bottaro, Mauro Carlin
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

import it.unipd.jandom.domains.numerical.utils.{MathLibrary => M}
import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import Box._

/**
  * The Box domain
  */

  // TODO: define reminder function

class BoxDomainCore extends CompleteLatticeOperator[Box]
  with IntOperator[Box] with Abstraction[Int,Box]{

  /**
    * @inheritdoc
    */
  def alpha(num : Int) : Box = Interval(num,num)

  /**
    * @inheritdoc
    */
  def sum(x : Box, y : Box) : Box = {
    (x,y) match {
      case (IntervalBottom, _) => IntervalBottom
      case (_, IntervalBottom) => IntervalBottom
      case (IntervalTop, _) => IntervalTop
      case (_, IntervalTop) => IntervalTop
      case (Interval(low1, high1), Interval(low2,high2)) => Interval((low1 + low2), (high1 + high2))
    }
  }

   /**
    * @inheritdoc
    */
  def inverse(x : Box) : Box = {
    x match {
      case (IntervalBottom) => IntervalBottom
      case (IntervalTop) => IntervalTop
      case (Interval (low, high)) => Interval (-high, -low)
    }
  }

   /**
    * @inheritdoc
    */
  def mult(x : Box, y : Box) : Box = {
    (x,y) match {
      case (IntervalBottom, _) => IntervalBottom
      case (_, IntervalBottom) => IntervalBottom
      case (IntervalTop, _) => IntervalTop
      case (_, IntervalTop) => IntervalTop
      case (Interval (low1, high1), Interval (low2,high2)) =>
        val comb = Array(low1 * low2, high1 * high2, low1 * high2, high1 * low2)
        val new_low = comb.reduceLeft(_ min _)
        val new_high = comb.reduceLeft(_ max _)
        Interval (new_low, new_high)
    }
  }

  /**
    * @inheritdoc
    */
  def division(x : Box, y : Box) : Box = {
    (x,y) match {
      case (IntervalBottom, _) => IntervalBottom
      case (_, IntervalBottom) => IntervalBottom
      case (IntervalTop, _) => IntervalTop
      case (_, IntervalTop) => IntervalTop
      case (Interval (low1, high1), Interval (low2,high2)) =>
        if (low2 >=1){
          val new_low = (low1 / low2) min (low1 / high2)
          val new_high = (high1 / low2) max (high1 / high2)
          Interval (new_low, new_high)
        } else if (high2 <= -1) {
          val new_low = (high1 / low2) min (high1 / high2)
          val new_high = (low1 / low2) max (low1 / high2)
          Interval (new_low, new_high)
        } else {
        // TODO: MODELLARE L'INFINITO "INTERO"
          Interval(1,1)
          //lub(division(Interval (low1, high1), glb(Interval (low2,high2), Interval(1,Double.PositiveInfinity)), division(Interval (low1, high1), glb(Interval (low2,high2), Interval(Double.NegativeInfinity, -1)))))
        }
    }
  }

  //TODO
  def remainder(x : Box, y : Box) : Box = {
    x
  }

  /**
    * @inheritdoc
    */
  def lub(x : Box, y : Box) : Box = {
    (x, y) match {
      case (IntervalTop, _) => IntervalTop
      case (_, IntervalTop) => IntervalTop
      case (IntervalBottom, _) => y
      case (_, IntervalBottom) => x
      case (Interval (low1, high1), Interval (low2, high2)) =>
        val new_low = low1 min low2
        val new_high = high1 max high2
        Interval (new_low, new_high)
    }
  }

  /**
    * @inheritdoc
    */
  def glb(x : Box, y : Box) : Box = {
    (x, y) match {
      case (IntervalTop, _) => y
      case (_, IntervalTop) => x
      case (IntervalBottom, _) => IntervalBottom
      case (_, IntervalBottom) => IntervalBottom
      case (Interval (low1, high1), Interval (low2, high2)) =>
        val new_low = low1 max low2
        val new_high = high1 min high2
        if (new_low > new_high) IntervalBottom else Interval (new_low, new_high)
    }
  }

  /**
    * @inheritdoc
    */
  def compare(x : Box, y : Box) : Option[Int] = {
    (x, y) match {
      case (IntervalBottom, IntervalBottom) => Option(0)
      case (IntervalBottom, _) => Option(-1)
      case (_, IntervalBottom) => Option(1)
      case (IntervalTop, IntervalTop) => Option(0)
      case (IntervalTop, _) => Option(1)
      case (_, IntervalTop) => Option(-1)
      case (Interval(low1, high1), Interval(low2, high2)) =>
        if (low1 == low2 && high1 == high2)
          Option(0)
        else if (low1 >= low2 && high1 <= high2)
          Option(-1)
        else if (low2 >= low1 && high2 >= high1)
          Option(1)
        else
          Option.empty
    }
  }

  /**
    * @inheritdoc
    */
  override def top: Box = IntervalTop

  /**
    * @inheritdoc
    */
  override def bottom: Box = IntervalBottom

}

object BoxDomainCore {
  /**
    * Factory method of BoxDomainCore
    */
  def apply() = new BoxDomainCore
} // end of BoxDomainCore companion object
