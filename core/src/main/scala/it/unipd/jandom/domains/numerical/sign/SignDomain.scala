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

package it.unipd.jandom.domains.numerical.sign

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.sign.Sign._
import it.unipd.jandom.domains.numerical._

/**
  * Sign domain, i.e. the domain composed of the elements Minus (negative numbers), Plus (positive numbers) and
  * Zero (0). SignTop and SignBottom complete the lattice, providing a greatest and a least element for this set.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *
 */
class SignDomain extends BaseNumericalDomain[Sign, SignDomainCore](sign.SignDomainCore()) {

  // this class uses the operations defined in SignDomainCore

  /**
    * @inheritdoc
    */
  override def createProperty(signsArray : Array[Sign], unreachable : Boolean): Property =
    new Property(signsArray, unreachable)

  /**
    * Numerical property that describes the sign of the variables in a certain point of the CFG.
    * @param sign array of the variables' signs
    * @param unreachable tells if a given program point is unreachable
    */
  type Property = SignProperty

  class SignProperty (sign : Array[Sign], unreachable: Boolean) extends BaseProperty(sign, unreachable) {

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm) : Property = {
      val s : Sign = linearEvaluation(lf)
      if (isEmpty)
        return this
      s match {
        case Plus => bottom
        case SignTop => top //IS this the best correct approximation???? //TODO Maybe we can improve this result Case Top????
        case SignBottom => bottom //TODO Check it
        case _ => this
      }
    }

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm) : Property = {
      if (isEmpty)
        return this
      val s : Sign = linearEvaluation(lf)
      s match {
        case Plus => this
        case Minus => this
        case Zero => bottom
        case SignTop => top //lub(Plus, Minus)
        case SignBottom => bottom
      }
    }

     /**
     * @inheritdoc
     */
    override def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (unreachable)
        "empty"
      else {
        val bounds = for (i <- 0 until dimension) yield {
          val h = sign(i) match {
            case SignTop => "TOP"
            case SignBottom => "BOTTOM"
            case Plus => "PLUS"
            case Minus => "Minus"
            case Zero => "Zero"
          }
          s"${vars(i)} = $h"
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }

  } // end of SignDomain's Property
} // end of SignDomain class

object SignDomain {
  /**
    * Ctor for SignDomain.
    */
  def apply() = new SignDomain()
} // end of SignDomain's companion object
