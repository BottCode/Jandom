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
import Sign._

/**
  * Operations on the (basic) sign domain.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class SignDomainCore extends CompleteLatticeOperator[Sign] with IntOperator[Sign] with Abstraction[Int, Sign] {

  /**
    * @inheritdoc
    */
  override def alpha(n : Int) : Sign = {
    if(n < 0) return Minus
    if(n == 0) return Zero
    Plus
  }

  /**
    * @inheritdoc
    */
  def sum(s: Sign, t: Sign) : Sign =
    (s,t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (SignTop, _) => SignTop
      case (_, SignTop) => SignTop
      case (a, Zero) => a
      case (Zero, a) => a
      case (Plus, Plus) => Plus
      case (Minus, Minus) => Minus
      case _ => SignTop
    }

  /**
    * @inheritdoc
    */
  def mult(s: Sign, t : Sign) : Sign =
    (s,t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (_, Zero) => Zero
      case (Zero, _) => Zero
      case (SignTop, _) => SignTop
      case (_, SignTop) => SignTop
      case (_, _) => if(s == t) Plus else Minus
    }

  /**
    * @inheritdoc
    */
  def inverse(s: Sign) : Sign =
    s match {
      case Plus => Minus
      case Minus => Plus
      case _ => s
    }

  /**
    * @inheritdoc
    */
  def division(s : Sign, t : Sign) : Sign =
    (s, t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (_, Zero) => SignBottom
      case (SignTop, _) => SignTop
      case (Zero, _) => Zero
      case (_, SignTop) => SignTop
      case _ => SignTop
    }

  /**
    * @inheritdoc
    */
  def remainder(s : Sign, t : Sign) : Sign =
    (s,t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (_, Zero) => SignBottom
      case (Zero, _) => Zero
      case (_, _) => SignTop
    }

  /**
    * @inheritdoc
    */
  def lub(s : Sign, t : Sign) : Sign =
    (s, t) match {
      case (SignTop, _) => SignTop
      case (_, SignTop) => SignTop
      case (SignBottom, _) => t
      case (_, SignBottom) => s
      case (_, _) => if (s == t) s else SignTop
    }

  /**
    * @inheritdoc
    */
  def glb(s : Sign, t : Sign) : Sign =
      (s,t) match {
        case (SignTop, _) => t
        case (_, SignTop) => s
        case (_, SignBottom) => SignBottom
        case (SignBottom, _) => SignBottom
        case (_, _) => if(s == t) s else SignBottom
      }

  /**
    * @inheritdoc
    */
  def compare(s: Sign, t:Sign): Option[Int] =
    (s, t) match {
      case (SignTop, SignTop) => Option(0)
      case (SignTop, _) => Option(1)
      case (_, SignTop) => Option(-1)
      case (SignBottom, SignBottom) => Option(0)
      case (SignBottom, _) => Option(-1)
      case (_, SignBottom) => Option(1)
      case (_, _) => if (s.equals(t)) Option(0) else Option.empty
    }

  /**
    * @inheritdoc
    */
  override def top: Sign = SignTop

  /**
    * @inheritdoc
    */
  override def bottom: Sign = SignBottom

} // end of SignDomainCore class

object SignDomainCore {
  def apply() : SignDomainCore = new SignDomainCore
} // end of SignDomainCore companion object
