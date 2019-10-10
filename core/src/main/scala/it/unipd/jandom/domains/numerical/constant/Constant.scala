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
package it.unipd.jandom.domains.numerical.constant

/**
  * The elements of the constant domain.
  * It is a flat domain composed of constant elements, top and bottom.
  * This domain is useful for the constant propagation analysis.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object Constant {
  sealed trait Constant

  // constant value
  case class Const (num : Int) extends Constant {
    override def toString : String = "= " + num
  }
  // no accurate info available for variable
  case object ConstantTop extends Constant {
    override def toString : String = "= \u22a4"
  }
  // no possible value
  case object ConstantBottom extends Constant {
    override def toString: String = "= \u22a5"
  }
} // end of Constant
