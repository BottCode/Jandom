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

package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.{LinearForm, NumericalDomain, NumericalProperty}
import it.unich.jandom.utils.numberext.RationalExt
import Mod3DomainCore._
import spire.math.Rational

import scala.math.PartiallyOrdered

class Mod3Domain extends BaseNumericalDomain[Mod3, numerical.Mod3Core.type](numerical.Mod3Core) {
  

  override def createProperty(mod3s: Array[Constant], unreachable: Boolean): Property =
    new Property(mod3s, unreachable)

  class Property (mod3s : Array[Constant], unreachable: Boolean) extends BaseProperty(mod3s, unreachable) {

    def apply(mod3s: Array[Mod3], unreachable: Boolean) : Property = new Property(mod3s, unreachable)

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm): Property = {
      if (isEmpty)
        return this
      val mod3 : Mod3 = linearEvaluation(lf)
      mod3 match {
        case Mod3Bottom => bottom
        case RestClass(0) => bottom
        case Mod3Top => top // lub
        case _ => this // mod3 != RestClass(0)
      }
    }

  } // end of Property
}
object Mod3Domain {
  def apply() = new Mod3Domain()
}
