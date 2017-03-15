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
import it.unipd.jandom.domains.numerical
import it.unipd.jandom.domains.numerical.{parity, _}

class ParityDomain extends BaseNumericalDomain[Parity, ParityDomainCore.type](parity.ParityDomainCore) {


  override def createProperty(elements: Array[Parity], unreachable: Boolean): Property =
    new Property(elements, unreachable)

  class Property (elements : Array[Parity], unreachable: Boolean) extends BaseProperty(elements, unreachable) {

    def apply(parity: Array[Parity], unreachable: Boolean) : Property = new Property(parity, unreachable)


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
    * Returns an abstract domain for boxes which is correct w.r.t. real arithmetic or
    * double arithmetic, according to the parameter `overReals`.
    */
  def apply() = new ParityDomain()

} // end of ParityDomain (companion object)
