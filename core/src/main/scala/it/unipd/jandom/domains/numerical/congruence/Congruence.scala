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
package it.unipd.jandom.domains.numerical.congruence

/**
  * The elements of the congruence domain.
  * It is a non-relational domain composed of values in the form `aZ + b` (Mod) and bottom (CongruenceBottom)
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object Congruence {
  sealed trait Congruence

  /**
    * Represents the elements of the domain of the form aZ + b.
    * Invariant: a = None (infinite) or a = Some(x) with x > 0
    * @param a modulus
    * @param b offset
    */
  case class Mod(a : Option[Int], b : Int) extends Congruence {
    /**Checks if `a` belongs to the set `N* U {∞}` as described in
      [[it.unipd.jandom.domains.numerical.congruence.CongruenceDomainCore]] */
    require(
      a match {
        case None => true
        case Some(x) => x > 0
      })

    /**
      * @inheritdoc
      */
    override def toString: String = {
      val s : String = "\u2208 "
      (a, b) match {
        case (None, 0) => s + "{0}"
        case (None, _) => s + "{" + b + "}"
        case (Some(1), 0) => s + "\u2124"
        case (Some(1), _) => s + "\u2124 + " + b
        case (Some(x), 0) => s + x + "\u2124"
        case (Some(x), _) => s + x + "\u2124 + " + b
      }
    }
  }

  /**
    * Corresponds to the bottom of the Congruence lattice
    */
  case object CongruenceBottom extends Congruence {
    /**
      * @inheritdoc
      */
    override def toString: String = "= \u22A5"
  }

} // end of Congruence object
