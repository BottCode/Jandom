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

import it.unich.jandom.domains.DomainTransformation
import it.unich.jandom.domains.numerical.ProductDomain
import it.unipd.jandom.domains.numerical.parity.ParityDomain
import it.unipd.jandom.domains.numerical.sign.ESeqDomain

/**
  * Instantiation of the reduced product domain using the Sign domain and the Parity domain.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  * @param dom1 the first abstract domain
  * @param dom2 the second abstract domain
  */
class ProductESeqParityDomain(override val dom1 : ESeqDomain, override val dom2 : ParityDomain) extends ProductDomain[ESeqDomain, ParityDomain](dom1, dom2) {
  override val dom1Todom2 = DomainTransformation.ESeqToParity
  override val dom2Todom1 = DomainTransformation.ParityToESeq

  /**
    * @inheritdoc
    */
  class ProductESeqParity(override val p1 : dom1.Property, override val p2 : dom2.Property) extends Product(p1, p2) {}

  /**
    * Class constructor
    */
  def apply(p1 : dom1.Property, p2 : dom2.Property) = new ProductESeqParity(p1, p2)

}  // end ProductESeqParityDomain class

object ProductESeqParityDomain {
  /**
    * Factory method
    */
  def apply() : ProductESeqParityDomain = new ProductESeqParityDomain(ESeqDomain(), ParityDomain())
} // end ProductESeqParityDomain companion object
