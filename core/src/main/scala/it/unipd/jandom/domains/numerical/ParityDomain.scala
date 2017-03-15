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

import it.unich.jandom.domains.numerical.LinearForm
import ParityDomainCore._
import it.unipd.jandom.domains.numerical

class ParityDomain extends BaseNumericalDomain[Parity, numerical.ParityDomainCore.type](numerical.ParityDomainCore) {


  override def createProperty(elements: Array[Parity], unreachable: Boolean): Property =
    new Property(elements, unreachable)

  class Property (elements : Array[Parity], unreachable: Boolean) extends BaseProperty(elements, unreachable) {

    def apply(parity: Array[Parity], unreachable: Boolean) : Property = new Property(parity, unreachable)

    /**
      * @inheritdoc
      */
    private def linearEvaluation(lf: LinearForm): Parity = {
      val known = lf.known.toDouble
      val homcoeffs = lf.homcoeffs.map(_.toDouble).toArray
      linearEvaluation(known, homcoeffs)
    }

    /**
      * @inheritdoc
      */
    private def linearEvaluation(known: Double, homcoeffs: Array[Double]): Parity = {
      require(homcoeffs.length <= dimension)
      if (unreachable && homcoeffs.exists { _ != 0 }) return ParityTop
      var p: Parity = toParity(known)
      for (i <- homcoeffs.indices)
        if (homcoeffs(i) != 0)
          p = sum(p, elements(i))
      p
    }

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
        case ParityTop => createProperty(Array.fill(dimension)(ParityDomainCore.top), false)
        case ParityBottom => createProperty(Array.fill(dimension)(ParityDomainCore.top), true)
      }
    }

    /**
      * @inheritdoc
      */
    override def linearInequality(lf: LinearForm): Property = linearDisequality(lf)

    /**
      * @inheritdoc
      */
    override def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (unreachable)
        "[ empty ]"
      else {
        val bounds = for (i <- 0 until dimension) yield {
          val h = elements(i) match {
            case Even => "EVEN"
            case Odd => "ODD"
            case ParityTop => "TOP"
            case ParityBottom => "BOTTOM"
          }
          s"${vars(i)} = $h"
        }
      bounds.mkString("[ ", " , ", " ]")
      }
    }

  } // end of Property

} // end of ParityDomain (class)

object ParityDomain {

  /**
    * Returns an abstract domain for boxes which is correct w.r.t. real arithmetic or
    * double arithmetic, according to the parameter `overReals`.
    */
  def apply() = new ParityDomain()

} // end of ParityDomain (companion object)
