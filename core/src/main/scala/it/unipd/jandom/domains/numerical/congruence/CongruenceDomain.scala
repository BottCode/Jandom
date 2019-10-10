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
package it.unipd.jandom.domains.numerical.congruence

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.BaseNumericalDomain
import it.unipd.jandom.domains.numerical.congruence.Congruence._
/**
  * Congruence domain as described in Mine 2002 [[https://hal.archives-ouvertes.fr/hal-00136663/document]].
  * The multiplication and remainder operator was taken and adapted from [[http://www.dsi.unive.it/~avp/domains.pdf]].
  * We based on these works because we could not retrieve the original paper by Granger [1989].
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *
  */
class CongruenceDomain extends BaseNumericalDomain[Congruence, CongruenceDomainCore](CongruenceDomainCore()) {

  /**
    * @inheritdoc
    */
  override def createProperty(congruences: Array[Congruence], unreachable: Boolean): Property =
    new Property(congruences, unreachable)

  class Property (congruences : Array[Congruence], unreachable: Boolean) extends BaseProperty(congruences, unreachable) {

    def apply(congruences: Array[Congruence], unreachable: Boolean) : Property = new Property(congruences, unreachable)

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm): Property = {
      if (isEmpty)
        return this
      val congruence : Congruence = linearEvaluation(lf)
      congruence match {
        case CongruenceBottom => bottom
        case Mod(None,0) => bottom //The result is the constant 0
        case _ => this // congruence != Const(0)
      }
    }

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm): Property = {
      val congruence: Congruence = linearEvaluation(lf)
      if (isEmpty)
        return this
      congruence match {
        case CongruenceBottom => bottom
        case Mod(None, b) => if (b > 0) bottom else this //The result is a constant
        case _ => this
      }
    }

    /**
      * Implementing the narrowing strategy described in
      * [[http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.220.7784&rep=rep1&type=pdf]] p. 56/68
      * @param that the abstract object to be narrowed with `this`. `that` IS assumed to be smaller than `this`.
      * @return the narrowing of the two abstract properties.
      */
    override def narrowing(that : Property) : Property =
      createProperty((this.elements, that.elements).zipped.map((x, y) => {
        x match {
          case Mod(Some(1), _) => y
          case _ => x
        }
      }), this.isEmpty && that.isEmpty)

  } // end of Property

} // end of CongruenceDomain (class)

object CongruenceDomain {

  def apply() = new CongruenceDomain()

} // end of CongruenceDomain (companion object)
