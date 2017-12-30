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
/**
  * Copyright 201K, 2016 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
  * JANDOM is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version K of the License, or
  * (at your option) any later version.
  *
  * JANDOM is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unipd.jandom.domains.numerical.mod

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.BaseNumericalDomain
import ModK.ModK

/**
  * Module K domain, i.e. the domain composed of the elements of
  * the rest class of K {0,..,K-1}.
  * It is a non-relational flat domain. ModKTop and ModKBottom complete
  * the lattice, providing a greatest and a least element for this set.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ModKDomain extends BaseNumericalDomain[ModK, ModKDomainCore.type](ModKDomainCore) {

  override def createProperty(modKs: Array[ModK], unreachable: Boolean): Property =
    new Property(modKs, unreachable)

  class Property (modKs : Array[ModK], unreachable: Boolean) extends BaseProperty(modKs, unreachable) {

    def apply(modKs: Array[ModK], unreachable: Boolean) : Property = new Property(modKs, unreachable)

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm): Property = this

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm): Property = this

  } // end of Property
} // end of ModKDomain class

object ModKDomain {

  /**
    * Factory method for ModKDomain.
    * Sets the divisor K to num invoking ModKDomainCore
    */
  def apply(num : Int) : ModKDomain = {
    ModKDomainCore(num)
    new ModKDomain()
  }
} // end of ModKDomain companion object