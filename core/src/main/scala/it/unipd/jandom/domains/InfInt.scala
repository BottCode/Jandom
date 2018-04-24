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
package it.unipd.jandom.domains

/**
  * This class manage integer operation avoid overflow.
  * It defines also a way to manage the "infinite" similar to Double.PositiveInfinity and Double.NegativeInfinity
  *
  * @author Mattia Bottaro <mattia.bottaro@studenti.unipd.it>
  * @author Mauro Carlin <mauro.carlin@studenti.unipd.it>
  */

trait InfInt {

  def isInfinity : Boolean

  /**
    * The following method are abstract.
    * The real implementation can be found in each sub-case class extending this trait
    */

  /**
    * Perform the mathematical sum between the object that invoke this method and the param
    * @param x is the adding
    * @return the mathematical sum between the object that invoke this method and x.
    */
  def +(x: InfInt): InfInt

  /**
    * Perform the mathematical division between the object that invoke this method and the param
    * @param x is the divider
    * @return the mathematical division between the object that invoke this method and x.
    */
  def /(x: InfInt): InfInt

  def *(x: InfInt): InfInt
  def >(x: InfInt): Boolean
  def >=(x: InfInt): Boolean

  /**
    * @param x is the second argument of max operator
    * @return the max element between the object that invoke this method and x.
    */
  def max(x: InfInt): InfInt

  def min(x: InfInt): InfInt
  def ==(x: InfInt): Boolean

  /**
    * @return the inverse of the object that invoke this method.
    */
  def inverse(): InfInt = {
    this match {
      case PositiveInfinity() => NegativeInfinity()
      case NegativeInfinity() => PositiveInfinity()
      case IntNumber(x) => IntNumber(-x)
    }
  }


  /**
    * Perform the mathematical difference between the object that invoke this method and the param
    * @param x is the substracting
    * @return the mathematical difference between the object that invoke this method and x.
    */
  def -(x: InfInt): InfInt = {
    this + x.inverse()
  }

  /**
    * Checks if two InfInt are not equal
    * @param x is the second argument of != operator
    * @return true if and only if the object that invoke this method is not equal to x.
    */
  def !=(x: InfInt): Boolean = {
    !(this == x)
  }

  /**
    * Checks if the object that invoke this method is less than the argument
    * @param x is the second argument of < operator
    * @return true if and only if the object that invoke this method is less than x.
    */
  def <(x: InfInt): Boolean = {
    !(this >= x)
  }

  // similar to <
  def <=(x: InfInt): Boolean = {
    !(this > x)
  }

}

case class IntNumber(n: Int) extends InfInt {

  def isInfinity = false

  def +(x: InfInt): InfInt = {
    x match {
      case NegativeInfinity() => NegativeInfinity()
      case PositiveInfinity() => PositiveInfinity()
      case IntNumber(m) => safeAdd(n,m)
    }
  }

  def *(x: InfInt): InfInt = {
    if (n == 0) return IntNumber(0)
    x match {
      case IntNumber(m) => safeMul(n,m)
      case _ =>
        if (n > 0)
          return x
        return x.inverse()
    }
  }

  def >(x: InfInt): Boolean = {
    x match {
      case PositiveInfinity() => false
      case IntNumber(m) => n > m
      case _ => true
    }
  }

  def >=(x: InfInt): Boolean = {
    x match {
      case IntNumber(m) => n >= m
      case PositiveInfinity() => false
      case _ => true
    }
  }

  def /(x: InfInt): InfInt = {
    x match {
      case IntNumber(m) => IntNumber(n / m)
      case _ => IntNumber(0)
    }
  }

  def ==(x: InfInt): Boolean = {
    x match {
      case IntNumber(x) => x == n
      case _ => false
    }
  }

  def max(x: InfInt): InfInt = {
    x match {
      case IntNumber(m) => IntNumber(n max m)
      case PositiveInfinity() => PositiveInfinity()
      case NegativeInfinity() => IntNumber(n)
    }
  }

  def min(x: InfInt): InfInt = {
    x match {
      case IntNumber(m) => IntNumber(n min m)
      case PositiveInfinity() => IntNumber(n)
      case NegativeInfinity() => NegativeInfinity()
    }
  }

  /**
    * Perform a "safe add", that is a sum that will never do an overflow.
    * @param x is the adding
    * @return the safe add between the object that invoke this method and x. If an overflow is detected, an Infinity is returned.
    */
  def safeAdd(left: Int, right: Int): InfInt = {
    if (right > 0 && left > Int.MaxValue - right)
      return PositiveInfinity()

    if (right < 0 && left < Int.MinValue - right)
      return NegativeInfinity()

    return IntNumber(left + right)
  }

  /**
    * Perform a "safe multiplication", that is a product that will never do an overflow.
    * @param x is the adding
    * @return the safe prduct between the object that invoke this method and x. If an overflow is detected, an Infinity is returned.
    */
  def safeMul(left: Int, right: Int): InfInt = { // TODO
    if (right > 0) {
      if (left > Int.MaxValue / right)
        return PositiveInfinity()
      if (left < Int.MinValue / right)
        return NegativeInfinity()
    }
    if (right < -1) {
      if (left > Int.MinValue / right)
        return NegativeInfinity()
      if (left < Int.MaxValue / right)
        return PositiveInfinity()
    }
    if (right == -1) {
      if (left == Int.MinValue)
        return PositiveInfinity()
      if (left == Int.MaxValue)
        return NegativeInfinity()
    }
    return IntNumber(left * right)
  }

  override def toString : String = n.toString
}

case class NegativeInfinity() extends InfInt {

  def isInfinity = true

  def +(x: InfInt): InfInt = {
    x match {
      case _ => NegativeInfinity()
    }
  }

  def *(x: InfInt): InfInt = {
    x match {
      case IntNumber(m) =>
        if (m > 0)
          return NegativeInfinity()
        if (m < 0)
          return PositiveInfinity()
        return IntNumber(0)
      case PositiveInfinity() => NegativeInfinity()
      case NegativeInfinity() => PositiveInfinity()
    }
  }

  def >(x: InfInt): Boolean = {
    x match {
      case NegativeInfinity() => false
      case _ => false
    }
  }

  def >=(x: InfInt): Boolean = {
    x match {
      case NegativeInfinity() => true
      case _ => false
    }
  }

  def /(x: InfInt): InfInt = {
    x match {
      case IntNumber(x) =>
        if(x < 0)
          return PositiveInfinity()
        return NegativeInfinity()
      case _ => IntNumber(0) // Inf / Inf = Inf * (1 / Inf) = Inf * 0
    }
  }

  def ==(x: InfInt): Boolean = {
    x match {
      case NegativeInfinity() => true
      case _ => false
    }
  }

  def max(x: InfInt): InfInt = {
    x match {
      case IntNumber(m) => IntNumber(m)
      case _ => x
    }
  }

  def min(x: InfInt): InfInt = {
    x match {
      case _ => NegativeInfinity()
    }
  }

  override def toString : String = "-\u221E"

}

case class PositiveInfinity() extends InfInt {

  def isInfinity = true

  def +(x: InfInt): InfInt = {
    x match {
      case _ => PositiveInfinity()
    }
  }

  def *(x: InfInt): InfInt = {
    x match {
      case IntNumber(x) =>
        if (x > 0)
          return PositiveInfinity()
        if (x < 0)
          return NegativeInfinity()
        return IntNumber(0)
      case _ => x
    }
  }

  def >(x: InfInt): Boolean = {
    x match {
      case NegativeInfinity() => true
      case _ => false
    }
  }

  def >=(x: InfInt): Boolean = {
    x match {
      case _ => true
    }
  }

  def ==(x: InfInt): Boolean = {
    x match {
      case PositiveInfinity() => true
      case _ => false
    }
  }

  def /(x: InfInt): InfInt = {
    x match {
      case IntNumber(m) =>
        if(m < 0)
          return NegativeInfinity()
        return PositiveInfinity()
      case _ => IntNumber(0) // Inf / Inf = Inf * (1 / Inf) = Inf * 0
    }
  }

  def max(x: InfInt): InfInt = {
    x match {
      case _ => PositiveInfinity()
    }
  }

  def min(x: InfInt): InfInt = {
    x match {
      case _ => x
    }
  }

  override def toString : String = "+\u221E"

}
