/**
 * Copyright 2017 Mattia Bottaro, Mauro Carlin
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

/**
  * The elements of the constant domain.
  * It is a flat domain composed of constant elements, top and bottom.
  * This domain is useful for the constant propagation analysis.
  *
  * @author Mattia Bottaro <mattia.bottaro@studenti.unipd.it>
  * @author Mauro Carlin <mauro.carlin@studenti.unipd.it>
  */
object Box {
  sealed trait Box

  // interval
  case class Interval (low : Int, high : Int) extends Box {
    override def toString : String = "= [" + low + "," + high + "]" 
  }
  // no accurate info available for variable
  case object IntervalTop extends Box {
    override def toString : String = "= \u22a4"
  }
  // no possible value
  case object IntervalBottom extends Box {
    override def toString: String = "= \u22a5"
  }
} 
