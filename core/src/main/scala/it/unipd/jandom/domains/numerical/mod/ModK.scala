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

/**
  * Domain with values with the form `k * Z + a`.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object ModK {
	private var k = 1

	sealed trait ModK
	/** Represents the elements in the equivalence class num modulo k */
	case class RestClass(num : Int) extends ModK {
    override def toString: String = k match {
      case 2 => num match {
        case 0 => "= Even"
        case 1 => "= Odd"
      }
      case _ => "\u2261 [" + num + "]%" + k
    }
	}
	case object ModKBottom extends ModK {
    override def toString: String = "= \u22A5"
  }
	case object ModKTop extends ModK {
    override def toString: String = "= \u22A4"
  }

	/**
	  * Singleton object constructor. Sets k to num
	  *
	  * @param num the value of the divisor k
	  */
	def apply(num : Int) {
		k = num
	}

	/** 
	  * Getter function for the divisor k
	  * @return the divisor k
	  */
	def divisor: Int = k

} // end of ModK object
