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
package it.unipd.jandom.domains.numerical.mod

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import ModK._

/**
  * Domain with values with the form `a + K * Z`.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object ModKDomainCore extends CompleteLatticeOperator[ModK]
  with IntOperator[ModK] with Abstraction[Int, ModK]{

  /**
    * @inheritdoc
    */
  override def alpha(t : Int) : ModK = {
    if(divisor == 0)
      return ModKTop      
    RestClass(((t % divisor) + divisor) % divisor)
  }

  /**
    * @inheritdoc
    */
  override def sum(m : ModK, n : ModK) : ModK = {
    (m, n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (ModKTop, _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (RestClass(m1), RestClass(n1)) =>
        alpha(m1+n1)
    }
  }

  /**
    * @inheritdoc
    */
  override def inverse(m : ModK) : ModK = {
    m match {
      case RestClass(n) => alpha((divisor - n) % divisor)
      case _ => m
    }
  }

  /**
    * @inheritdoc
    */
  override def mult(m : ModK, n : ModK) : ModK = {
    (m, n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (RestClass(0), _) => RestClass(0)
      case (_, RestClass(0)) => RestClass(0)
      case (ModKTop, _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (RestClass(m1), RestClass(n1)) =>
        alpha(m1*n1)
    }
  }

  /**
    * @inheritdoc
    */
  override def division(m : ModK, n : ModK) : ModK = {
    (m,n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (ModKTop, _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (_, _) => ModKTop
    }
  }

  /**
    * @inheritdoc
    */
  override def remainder(m : ModK, n : ModK) : ModK = {
    (m,n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (_, RestClass(0)) => ModKTop
      case (RestClass(0), _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (ModKTop, _) => ModKTop
      case (RestClass(m1), RestClass(n1)) => alpha(m1%n1)
    }
  }

  /**
    * @inheritdoc
    */
  override def lub(m : ModK, n : ModK) : ModK = {
    (m,n) match {
      case (ModKTop, _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (ModKBottom, _) => n
      case (_, ModKBottom) => m
      case (_, _) => if(m == n) m else ModKTop
    }
  }

  /**
    * @inheritdoc
    */
  override def glb(m : ModK, n : ModK) : ModK = {
    (m, n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (ModKTop, _) => n
      case (_, ModKTop) => m
      case (_, _) => if(m == n) m else ModKBottom
    }
  }

  /**
    * @inheritdoc
    */
  override def compare(m : ModK, n : ModK) : Option[Int] = {
    (m,n) match {
      case (ModKTop, ModKTop) => Option(0)
      case (ModKTop, _) => Option(1)
      case (_, ModKTop) => Option(-1)
      case (ModKBottom, ModKBottom) => Option(0)
      case (ModKBottom, _) => Option(-1)
      case (_, ModKBottom) => Option(1)
      case (_, _) => if(m == n) Option(0) else Option.empty
    }
  }

  /**
    * @inheritdoc
    */
  override def top: ModK = ModKTop

  /**
    * @inheritdoc
    */
  override def bottom: ModK = ModKBottom

  def apply(num : Int) = ModK(num)

} // end of ModKDomainCore companion object
