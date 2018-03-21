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
import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator, InfInt, IntNumber}
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
    print("Astraggo ",num)
    return Interval(IntNumber(num),IntNumber(num))
  }

  /**
    * @inheritdoc
    */
  def sum(x : Box, y : Box) : Box = {
    print("sum",x,y)
    (x,y) match {
      case (IntervalBottom, _) => IntervalBottom
      case (_, IntervalBottom) => IntervalBottom
      case (IntervalTop, _) => IntervalTop
      case (_, IntervalTop) => IntervalTop
      case (Interval(low1,high1),Interval(low2,high2)) => Interval(low1 + low2, high1 + high2)
    }
  }

   /**
    * @inheritdoc
    */
  def inverse(x : Box) : Box = {
    x
    // print("INVERSE")
    // x match {
    //   case (IntervalBottom) => IntervalBottom
    //   case (IntervalTop) => IntervalTop
    //   case (IntervalNegative(high)) => IntervalPositive(-high)
    //   case (IntervalPositive(low)) =>
    //     if (low == InfInt.MinValue)
    //       return IntervalTop
    //     return IntervalNegative(-low)
    //   case (Interval(low,high)) =>
    //     if (low == InfInt.MinValue)
    //       IntervalPositive(-high)
    //     Interval(-high,-low)
    // }
  }

   /**
    * @inheritdoc
    */
  /*
  def mult(x : Box, y : Box) : Box = {
    (x,y) match {
      case (IntervalBottom, _) => IntervalBottom
      case (_, IntervalBottom) => IntervalBottom
      case (IntervalTop, _) => IntervalTop
      case (_, IntervalTop) => IntervalTop
      case (IntervalNegative(_), IntervalPositive(_)) => IntervalTop
      case (IntervalPositive(_), IntervalNegative(_)) => IntervalTop
      case (IntervalNegative(high1), IntervalNegative(high2)) =>
        val res = safeMult(high1,high2)
        if (res < InfInt.MaxValue && res > InfInt.MinValue)
          return IntervalPositive(res)
        return IntervalTop

      case(IntervalNegative(high1), Interval(low2,high2)) =>
        val res = Array(safeMult(high1,low2), safeMult(high1,high2))
        val min_res = res.reduceLeft(_ min _)
        val max_res = res.reduceLeft(_ max _)

        if (low2 < 0) {
          if (min_res < InfInt.MaxValue && min_res > InfInt.MinValue)
            return IntervalPositive(min_res)
          return IntervalTop
        }
        if (low2 > 0) {
          if (high1 > 0)
        }
        if (res < InfInt.MaxValue && res > InfInt.MinValue)
          return IntervalTop
        return IntervalPositive(res)

      case(IntervalPositive(low1), IntervalPositive(low2)) =>


      case(IntervalPositive(low1), Interval(low2,high2)) =>

      case (Interval (low1, high1), Interval (low2,high2)) =>
        val comb = Array(safeMult(low1, low2), safeMult(high1, high2), safeMult(low1, high2), safeMult(high1, low2))
        val new_low = comb.reduceLeft(_ min _)
        val new_high = comb.reduceLeft(_ max _)
        Interval (new_low, new_high)
    }
  }
*/
/*
def mult(x : Box, y : Box) : Box = {
  var bounds = Array(projectLow(x),projectHigh(x),projectLow(y),projectHigh(y))
  for(i <- bounds.indices){
    if(bounds(i).toInt == InfInt.MinValue) bounds(i) = Double.NegativeInfinity
    else if(bounds(i).toInt == InfInt.MaxValue) bounds(i) = Double.PositiveInfinity
  }
  var comb = Array(bounds(0)*bounds(2),bounds(0)*bounds(3),bounds(1)*bounds(2),bounds(1)*bounds(3))
  var left = comb.reduceLeft(_ min _)
  var right = comb.reduceLeft(_ max _)
  if(left == Double.NegativeInfinity && (right == Double.PositiveInfinity || right == Double.NegativeInfinity)) return IntervalTop
  if(left == Double.NegativeInfinity) return IntervalNegative(right.toInt)
  

}
*/
  
  def mult(x : Box, y : Box) : Box = {
    x
  }

  /**
    * @inheritdoc
    */
  def division(x : Box, y : Box) : Box = {
    x
    // (x,y) match {
    //   case (IntervalBottom, _) => IntervalBottom
    //   case (_, IntervalBottom) => IntervalBottom
    //   case (IntervalTop, _) => IntervalTop
    //   case (_, IntervalTop) => IntervalTop
    //   case (Interval (low1, high1), Interval (low2,high2)) =>
    //     if (low2 >= 1) {
    //       val new_low = (low1 / low2) min (low1 / high2)
    //       val new_high = (high1 / low2) max (high1 / high2)
    //       Interval (new_low, new_high)

    //     } else if (high2 <= -1) {
    //       val new_low = (high1 / low2) min (high1 / high2)
    //       val new_high = (low1 / low2) max (low1 / high2)
    //       Interval (new_low, new_high)

    //     } else {
    //     // TODO: MODELLARE L'INFINITO "INTERO"
    //       Interval(1,1)
    //       //lub(division(Interval (low1, high1), glb(Interval (low2,high2), Interval(1,InfInt.MaxValue)), division(Interval (low1, high1), glb(Interval (low2,high2), Interval(InfInt.MinValue, -1)))))
    //     }
    // }
  }

  //TODO
  def remainder(x : Box, y : Box) : Box = {
    x
  }

  /**
    * @inheritdoc
    */
  def lub(x : Box, y : Box) : Box = {
    x
    // (x, y) match {
    //   case (IntervalTop, _) => IntervalTop
    //   case (_, IntervalTop) => IntervalTop
    //   case (IntervalBottom, _) => y
    //   case (_, IntervalBottom) => x
    //   case (Interval (low1, high1), Interval (low2, high2)) =>
    //     val new_low = low1 min low2
    //     val new_high = high1 max high2
    //     Interval (new_low, new_high)
    // }
  }

  /**
    * @inheritdoc
    */
  def glb(x : Box, y : Box) : Box = {
    x
    // (x, y) match {
    //   case (IntervalTop, _) => y
    //   case (_, IntervalTop) => x
    //   case (IntervalBottom, _) => IntervalBottom
    //   case (_, IntervalBottom) => IntervalBottom
    //   case (Interval (low1, high1), Interval (low2, high2)) =>
    //     val new_low = low1 max low2
    //     val new_high = high1 min high2
    //     if (new_low > new_high) IntervalBottom else Interval (new_low, new_high)
    // }
  }

  /**
    * @inheritdoc
    */
  def compare(x : Box, y : Box) : Option[Int] = {
    Option(0)
    // (x, y) match {
    //   case (IntervalBottom, IntervalBottom) => Option(0)
    //   case (IntervalBottom, _) => Option(-1)
    //   case (_, IntervalBottom) => Option(1)
    //   case (IntervalTop, IntervalTop) => Option(0)
    //   case (IntervalTop, _) => Option(1);
    //   case (_, IntervalTop) => Option(-1);
    //   case (Interval(low1, high1), Interval(low2, high2)) =>
    //     if (low1 == low2 && high1 == high2)
    //       Option(0)
    //     else if (low1 >= low2 && high1 <= high2)
    //       Option(-1)
    //     else if (low2 >= low1 && high2 <= high1)
    //       Option(1)
    //     else
    //       Option.empty
    // }
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
