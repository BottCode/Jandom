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

class Mod3Domain extends NumericalDomain {

  def apply(mod3s: Array[Mod3]): Property = Property(mod3s, mod3s.forall(p => p.equals(Mod3Bottom)))

  /**
    * @inheritdoc
    */
  def bottom(n: Int) = Property(Array.fill(n)(Mod3Bottom), unreachable = true)

  /**
    * @inheritdoc
    */
  def top(n: Int) = Property(Array.fill(n)(Mod3Top), unreachable = false)


  case class Property private[Mod3Domain](parity : Array[Mod3], unreachable: Boolean) extends NumericalProperty[Property] {
    type Domain = Mod3Domain

    def domain = Mod3Domain.this
    
  } // end of Property
}
object Mod3Domain {
  def apply() = new Mod3Domain()
}
