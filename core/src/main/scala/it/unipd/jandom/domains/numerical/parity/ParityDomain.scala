/**
  * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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
  * You should have received a copy of the GNU General Public License
  * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unipd.jandom.domains.numerical.parity

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.{parity, _}

/**
  * Parity domain, i.e. the domain composed of the elements Even (even numbers) and Odd (odd numbers). SignTop and
  * SignBottom complete the lattice, providing a greatest and a least element for this set.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *
  */
class ParityDomain extends BaseNumericalDomain[Parity, ParityDomainCore.type](parity.ParityDomainCore) {

  // this class uses the operations defined in ParityDomainCore

  /**
    * @inheritdoc
    */
  override def createProperty(elements: Array[Parity], unreachable: Boolean): Property =
    new Property(elements, unreachable)

  /**
    * Numerical property that describes the parity of the variables in a certain point of the CFG.
    * @param elements array of the variables' parities
    * @param unreachable tells if a given program point is unreachable
    */
  class Property (elements : Array[Parity], unreachable: Boolean) extends BaseProperty(elements, unreachable) {

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm): Property = {
      if (unreachable)
        return this
      val p: Parity = linearEvaluation(lf)
      p match {
        case Even => this
        case Odd => this
        case ParityTop => createProperty(Array.fill(dimension)(ParityDomainCore.top), unreachable = false)
        case ParityBottom => createProperty(Array.fill(dimension)(ParityDomainCore.top), unreachable = true)
      }
    }

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm): Property = linearDisequality(lf)

  } // end of Property
} // end of ParityDomain (class)

object ParityDomain {
  /**
    * Ctor for ParityDomain.
    */
  def apply() = new ParityDomain()
} // end of ParityDomain (companion object)
