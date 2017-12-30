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


/**
  * The elements of the extended sign domain with 0 and 1,
  * It is a flat domain which does not inherit from Sign
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object ES01 {
  trait ExtendedSign01
  // negative numbers (< 0)
  case object Negative extends ExtendedSign01 {
    override def toString = "< 0"
  }
  // null numbers (= 0)
  case object Zero extends ExtendedSign01 {
    override def toString: String = "= 0"
  }
  // numbers equal to 1 (= 1)
  case object One extends ExtendedSign01 {
    override def toString: String = "= 1"
  }
  // numbers greater than 1 (> 1)
  case object GTOne extends ExtendedSign01 {
    override def toString: String = "> 1"
  }
  // no accurate info available for variable
  case object ES01Top extends ExtendedSign01 {
      override def toString: String = "= \u22A4"
  }
  // no possible value
  case object ES01Bottom extends ExtendedSign01 {
    override def toString: String = "= \u22A5"
  }
} // end of ES01 object
