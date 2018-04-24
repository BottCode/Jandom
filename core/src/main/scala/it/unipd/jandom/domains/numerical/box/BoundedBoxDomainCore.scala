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
import it.unipd.jandom.domains.{InfInt, IntNumber, PositiveInfinity, NegativeInfinity}
import Box._

/*
  * The bounded Box domain
  * @author Mattia Bottaro <mattia.bottaro@studenti.unipd.it>,
  * @author Mauro Carlin <mauro.carlin@studenti.unipd.it>
*/

class BoundedBoxDomainCore(bound_m : InfInt, bound_n : InfInt) extends BoxDomainCore{

  var m: InfInt = bound_m
  var n: InfInt = bound_n

  override def sum(x : Box, y : Box) : Box = {
    val result = super.sum(x,y)
    normalizeBound(result)
  }

  /**
    * @inheritdoc
    */
  override def inverse(x : Box) : Box = {
    val result = super.inverse(x)
    normalizeBound(result)
  }

  /**
    * @inheritdoc
    */
  override def mult(x : Box, y : Box) : Box = {
    val result = super.mult(x,y)
    normalizeBound(result)
  }

  /**
    * @inheritdoc
    */
  override def division(x : Box, y : Box) : Box = {
    val result = super.division(x,y)
    normalizeBound(result)
  }

  /**
    * @inheritdoc
    */
  override def remainder(x : Box, y : Box) : Box = {
    val result = super.remainder(x,y)
    normalizeBound(result)
  }

  /**
    * @inheritdoc
    */
  override def lub(x : Box, y : Box) : Box = {
    val result = super.lub(x,y)
    normalizeBound(result)
  }

  /**
    * @inheritdoc
    */
  override def glb(x : Box, y : Box) : Box = {
    val result = super.glb(x,y)
    normalizeBound(result)
  }

  def normalizeBound(x : Box) : Box = {
    x match {
      case Interval(low,high) => {
        if (low == high)
          return Interval(low,high)

        if (n > m) {
          if (high < m)
            return Interval(NegativeInfinity(),m)
          if (low > n)
            return Interval(n,PositiveInfinity())

          var new_low = low
          var new_high = high
          if (high > n)
            new_high = PositiveInfinity()
          if (low < m)
            new_low = NegativeInfinity()

          return check(Interval(new_low,new_high))
        }
        return IntervalTop
      }
      case _ => x

    }
  }

} // end of BoundedBoxDomainCore class

object BoundedBoxDomainCore {
  /**
    * Factory method of BoundedBoxDomainCore
    */
  var D: BoundedBoxDomainCore = apply(IntNumber(0),IntNumber(0))
  def apply(m : InfInt, n : InfInt): BoundedBoxDomainCore = {
    D = new BoundedBoxDomainCore(m,n)
    return D
  }

  def updateData(x: InfInt, y: InfInt) = {
    D.m = x
    D.n = y
  }

  def norm(x : Box) : Box = {
    D.normalizeBound(x)
  }

} // end of BoundedBoxDomainCore companion object
