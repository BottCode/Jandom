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
import Sign._

/**
  * The elements of the extended sign domain
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object ESeq {
  // numbers greater or equal than 0 (>= 0)
  case object Geq0 extends Sign {
    override def toString: String = "\u2265 0"
  }
  // numbers less or equal than 0 (<= 0)
  case object Leq0 extends Sign {
    override def toString: String = "\u2264 0"
  }
  // numbers not equal to 0 (!= 0)
  case object Neq0 extends Sign {
    override def toString: String = "\u2260 0"
  }
} // end of ESeq object
