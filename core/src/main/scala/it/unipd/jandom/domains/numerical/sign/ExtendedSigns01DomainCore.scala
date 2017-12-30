/**
 * Copyright 2017 Mirko Bez, Stefano Munari, Sebastiano Valle
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
package it.unipd.jandom.domains.numerical.sign

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import ES01._

/**
  * Operations on the sign domain extended with the element 1.
  * This domain is intended to be used for analysis on integer variables.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ExtendedSigns01DomainCore extends Abstraction[Int, ExtendedSign01]
  with IntOperator[ExtendedSign01] with CompleteLatticeOperator[ExtendedSign01] {

  /**
    * @inheritdoc
    */
  override def alpha(n : Int) : ExtendedSign01 = {
    if (n == 0) return Zero
    if (n == 1) return One
    if (n < 0)  return Negative
    GTOne
  }

  /**
    * @inheritdoc
    */
  def sum(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (ES01Top, _) => ES01Top
      case (_, ES01Top) => ES01Top
      case (Negative, Negative) => Negative
      case (_, Zero) => s
      case (Zero, _) => t
      case (One, One) => GTOne
      case (GTOne, One) => GTOne
      case (One, GTOne) => GTOne
      case (GTOne, GTOne) => GTOne
      case _ => ES01Top
    }
  }

  /**
    * @inheritdoc
    */
  def mult(s : ExtendedSign01, t : ExtendedSign01): ExtendedSign01 = {
    (s, t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (_, Zero) => Zero
      case (Zero, _) => Zero
      case (_, One) => s
      case (One, _) => t
      case (Negative, Negative) => ES01Top
      case (Negative, GTOne) => Negative
      case (GTOne, Negative) => Negative
      case (GTOne, GTOne) => GTOne
      case _ => ES01Top
    }
  }

  /**
    * @inheritdoc
    */
    def inverse(s : ExtendedSign01): ExtendedSign01 = {
      s match {
        case GTOne => Negative
        case One => Negative
        case Negative => ES01Top
        case _ => s
      }
  }

  /**
    * @inheritdoc
    */
  def division(s : ExtendedSign01, t : ExtendedSign01): ExtendedSign01 = {
    (s,t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (_, Zero) => ES01Bottom
      // we already took care of a / 0
      case (Zero, _) => Zero
        // a / 1 = a
      case (_, One) => s
      case (One, GTOne) => Zero
      // (2 / 5) = 0   ---   (5 / 5) = 1   ---   (10 / 5) = 2
      case (_, GTOne) => ES01Top
      // (-2 / -5) = 0   ---   (-4 / -2) = 2
      // (1 / -5) = 0   ---   (1 / -1) = -1
      // (5 / -5) = -1   ---   (2 / -5) = 0
      case (_, Negative) => ES01Top
      case _ => ES01Top
    }
  }

  /**
    * @inheritdoc
    */
  def remainder(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Bottom, _) => ES01Bottom
      case (_, ES01Bottom) => ES01Bottom
      case (_, Zero) => ES01Bottom
      case (_, One) => Zero
      case (Zero, _) => Zero
      case (One, GTOne) => One
      case _ => ES01Top
    }
  }

  /**
    * @inheritdoc
    */
  def lub(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Top, _) => ES01Top
      case (_, ES01Top) => ES01Top
      case (_, ES01Bottom) => s
      case (ES01Bottom, _) => t
      case (_, _) => if (s.equals(t)) s else ES01Top
    }
  }

  /**
    * @inheritdoc
    */
  def glb(s : ExtendedSign01, t : ExtendedSign01) : ExtendedSign01 = {
    (s,t) match {
      case (ES01Top, _) => t
      case (_, ES01Top) => s
      case (_, ES01Bottom) => ES01Bottom
      case (ES01Bottom, _) => ES01Bottom
      case (_,_) => if(s.equals(t)) s else ES01Bottom
    }
  }

  /**
    * @inheritdoc
    */
  def compare(s : ExtendedSign01, t : ExtendedSign01) : Option[Int] = {
    (s,t) match {
      case (ES01Top, ES01Top) => Option(0)
      case (ES01Bottom, ES01Bottom) => Option(0)
      case (ES01Top, _) => Option(1)
      case (_, ES01Top) => Option(-1)
      case (ES01Bottom, _) => Option(-1)
      case (_, ES01Bottom) => Option(1)
      case (_,_) => if(s.equals(t)) Option(0) else Option.empty
    }
  }

  /**
    * @inheritdoc
    */
  override def top: ExtendedSign01 = ES01Top

  /**
    * @inheritdoc
    */
  override def bottom: ExtendedSign01 = ES01Bottom
} // end of ExtendedSigns01DomainCore class

object ExtendedSigns01DomainCore {
  def apply() = new ExtendedSigns01DomainCore
} // end of ExtendedSigns01DomainCore companion object
