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
import it.unipd.jandom.domains.numerical.{BaseNumericalDomain}
import ModK._

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

    /**
      * @inheritdoc
      */
    override def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (unreachable)
        "[ empty ]"
      else {
        val bounds = for (i <- 0 until dimension) yield {
          val c = modKs(i) match {
            case ModKTop => "TOP"
            case ModKBottom => "BOTTOM"
            case RestClass(r) => s"$r mod ${divisor}"
          }
          s"${vars(i)} = $c"
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }
  } // end of Property
} // end of ModKDomain class

object ModKDomain {
  def apply(num : Int) = {
    ModKDomainCore(num)
    new ModKDomain()
  }
} // end of ModKDomain companion object
