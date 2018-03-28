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

import it.unipd.jandom.domains.numerical.utils.{MathLibrary => M}
import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator, InfInt, IntNumber, PositiveInfinity, NegativeInfinity, Undetermined}
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
  def alpha(num : Int) : Box = {
    return Interval(IntNumber(num),IntNumber(num))
  }

  /**
    * @inheritdoc
    */
  def sum(x : Box, y : Box) : Box = {
    (x,y) match {
      case (IntervalBottom, _) => IntervalBottom
      case (_, IntervalBottom) => IntervalBottom
      case (IntervalTop, _) => IntervalTop
      case (_, IntervalTop) => IntervalTop
      case (Interval(low1,high1),Interval(low2,high2)) => check(Interval(low1 + low2, high1 + high2))
    }
  }

   /**
    * @inheritdoc
    */
  def inverse(x : Box) : Box = {
    x match {
      case IntervalBottom => IntervalBottom
      case IntervalTop => IntervalTop
      case Interval(low,high) => Interval(high.inverse(),low.inverse()) // TODO, bruttino
    }
  }

   /**
    * @inheritdoc
    */
  def mult(x : Box, y : Box) : Box = {
    (x,y) match {
      case (IntervalBottom, _) => IntervalBottom
      case (_, IntervalBottom) => IntervalBottom
      case (_, Interval(IntNumber(0),IntNumber(0))) => Interval(IntNumber(0),IntNumber(0))
      case (Interval(IntNumber(0),IntNumber(0)), _) => Interval(IntNumber(0),IntNumber(0))
      case (IntervalTop, _) => IntervalTop
      case (_, IntervalTop) => IntervalTop
      case (Interval(low1,high1),Interval(low2,high2)) =>
        val comb = Array(low1 * low2, high1 * high2, low1 * high2, high1 * low2)
        val new_low = comb.reduceLeft(_ min _)
        val new_high = comb.reduceLeft(_ max _)
        return check(Interval(new_low,new_high))
    }
  }

  def check(x : Interval) : Box = {
    x match {
      case Interval(Undetermined(), _) => IntervalTop
      case Interval(_, Undetermined()) => IntervalTop
      case Interval(NegativeInfinity(), PositiveInfinity()) => IntervalTop
      case _ => x
    }
  }

  /**
    * @inheritdoc
    */
  def division(x : Box, y : Box) : Box = {
    (x,y) match {
      case (IntervalBottom, _) => IntervalBottom
      case (_, IntervalBottom) => IntervalBottom
      case (_, Interval(IntNumber(0),IntNumber(0))) => IntervalBottom
      case (IntervalTop, _) => IntervalTop
      case (_, IntervalTop) => IntervalTop
      case (Interval (low1, high1), Interval (low2,high2)) =>
        if (low2 >= IntNumber(1)) {
          val new_low = (low1 / low2) min (low1 / high2)
          val new_high = (high1 / low2) max (high1 / high2)
          return Interval (new_low, new_high)
        }
        if (IntNumber(-1) >= high2) {
          val new_low = (high1 / low2) min (high1 / high2)
          val new_high = (low1 / low2) max (low1 / high2)
          return Interval (new_low, new_high)
        }

        val one_to_infinite = Interval(IntNumber(1),PositiveInfinity())
        val minus_infinite_to_minus_one = Interval(NegativeInfinity(),IntNumber(-1))
        return lub (
                  division(Interval (low1, high1), glb(Interval (low2,high2), one_to_infinite)),              // first argument
                  division(Interval (low1, high1), glb(Interval (low2,high2), minus_infinite_to_minus_one)) // second argument
                )
    }
  }

  //TODO
  def remainder(x : Box, y : Box) : Box = {
    sum(x,inverse(mult(division(x,y),y))) // (10%4) = 10 - (10/4)*4
  }

  /**
    * @inheritdoc
    */
  def lub(x : Box, y : Box) : Box = {
    (x, y) match {
      case (IntervalTop, _) => return IntervalTop
      case (_, IntervalTop) => return IntervalTop
      case (IntervalBottom, _) => return y
      case (_, IntervalBottom) => return x
      case (Interval (low1, high1), Interval (low2, high2)) =>
        val new_low = low1 min low2
        val new_high = high1 max high2
        return Interval (new_low, new_high)
    }
  }

  /**
    * @inheritdoc
    */
  def glb(x : Box, y : Box) : Box = {
    print("GLB",x,y)
    (x, y) match {
      case (IntervalTop, _) => return y
      case (_, IntervalTop) => return x
      case (IntervalBottom, _) => return IntervalBottom
      case (_, IntervalBottom) => return IntervalBottom
      case (Interval (low1, high1), Interval (low2, high2)) =>
        val new_low = low1 max low2
        val new_high = high1 min high2
        if (new_low > new_high)
          return IntervalBottom

        return Interval (new_low, new_high)
    }
  }

  /**
    * @inheritdoc
    */
  def compare(x : Box, y : Box) : Option[Int] = {
    (x, y) match {
      case (IntervalBottom, IntervalBottom) => return Option(0)
      case (IntervalBottom, _) => return Option(-1)
      case (_, IntervalBottom) => return Option(1)
      case (IntervalTop, IntervalTop) => return Option(0)
      case (IntervalTop, _) => return Option(1);
      case (_, IntervalTop) => return Option(-1);
      case (Interval(low1, high1), Interval(low2, high2)) =>
        if (low1 == low2 && high1 == high2)
          return Option(0)
        if (low1 >= low2 && high2 >= high1)
          return Option(-1)
        if (low2 >= low1 && high1 >= high2)
          return Option(1)

        return Option.empty
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
