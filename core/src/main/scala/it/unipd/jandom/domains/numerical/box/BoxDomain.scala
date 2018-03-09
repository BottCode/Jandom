/**
 * Copyright 2018 Mattia Bottaro, Mauro Carlin
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

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.BaseNumericalDomain
import it.unipd.jandom.domains.numerical.box.Box._
import it.unipd.jandom.domains.numerical.box.BoxDomainCore._


/**
  * Box Domain ... @TODO a better explanation is required
  *
  * @author Mattia Bottaro <mattia.bottaro@studenti.unipd.it>
  * @author Mauro Carlin <mauro.carlin@studenti.unipd.it>
  */
class BoxDomain extends BaseNumericalDomain[Box, BoxDomainCore](BoxDomainCore()) {

  /**
    * @inheritdoc
    */
  override def createProperty(boxes: Array[Box], unreachable: Boolean): Property =
    new Property(boxes, unreachable)

  /**
    * Numerical property that tells whether the variables in a certain point of the CFG are constant or not.
    * @param boxes array of the variables' boxes status
    * @param unreachable tells if a given program point is unreachable
    */
  class Property (boxes : Array[Box], unreachable: Boolean) extends BaseProperty(boxes, unreachable) {

	def apply(boxes: Array[Box], unreachable: Boolean) : Property = new Property(boxes, unreachable)
    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm): Property = {
      
    }

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm): Property = {
      
    }
    
    override def widening(that : Property) : Property = {
	
	}
	
	override def narrowing(that : Property) : Property = {
		
	}

  } // end of Property
} // end of BoxDomain (class)

object BoxDomain {
  /**
    * Factory method of BoxDomain
    */
  def apply() = new BoxDomain()
} 
