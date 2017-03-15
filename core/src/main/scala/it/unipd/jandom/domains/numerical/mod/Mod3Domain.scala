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

package it.unipd.jandom.domains.numerical.mod

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.{BaseNumericalDomain}

class Mod3Domain extends BaseNumericalDomain[Mod3, Mod3DomainCore.type](Mod3DomainCore) {

  override def createProperty(mod3s: Array[Mod3], unreachable: Boolean): Property =
    new Property(mod3s, unreachable)

  class Property (mod3s : Array[Mod3], unreachable: Boolean) extends BaseProperty(mod3s, unreachable) {

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

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm): Property = {
      val mod3: Mod3 = linearEvaluation(lf)
      if (isEmpty)
        return this
      mod3 match {
        case Mod3Bottom => bottom
        case Mod3Top => top
        case RestClass(r) => if (r > 0) bottom else this
      }
    }

    /**
      * @inheritdoc
      */
    override def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (unreachable)
        "[ empty ]"
      else {
        val bounds = for (i <- 0 until dimension) yield {
          val c = mod3s(i) match {
            case Mod3Top => "TOP"
            case Mod3Bottom => "BOTTOM"
            case RestClass(r) => r
          }
          s"${vars(i)} = $c"
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }

  } // end of Property
}
object Mod3Domain {
  def apply() = new Mod3Domain()
}
