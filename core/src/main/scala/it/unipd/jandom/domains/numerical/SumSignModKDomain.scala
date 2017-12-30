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
package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.numerical.SumDomain
import it.unipd.jandom.domains.numerical.mod.ModKDomain
import it.unipd.jandom.domains.numerical.sign.SignDomain

/**
  * Instantiation of the sum domain using the Sign domain and the Parity domain
  * (ModK with K=2).
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  * @param dom1 the first abstract domain
  * @param dom2 the second abstract domain
  */
class SumSignModKDomain(val dom1: SignDomain, val dom2: ModKDomain) extends SumDomain[SignDomain, ModKDomain]{

  type Property = SumSignModK

  /**
    * @inheritdoc
    */
  class SumSignModK(val p1: dom1.Property, val p2: dom2.Property) extends Sum {}

  /**
    * Class constructor
    */
  def apply(p1: dom1.Property, p2: dom2.Property) = new SumSignModK(p1, p2)
} // end SumSignParityDomain class

object SumSignModKDomain {
  /**
    * Factory method
    */
  def apply(num : Int) : SumSignModKDomain = new SumSignModKDomain(SignDomain(), ModKDomain(num))
} // end of SumSignModKDomain companion object
