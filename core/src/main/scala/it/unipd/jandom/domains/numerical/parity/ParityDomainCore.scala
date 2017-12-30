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
package it.unipd.jandom.domains.numerical.parity

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}

import Parity._

/**
  * Domain with values with the form `a + 2 * Z`.
  * Degenerate case of the Congruence domain (the one with modulus always equal to 2).
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ParityDomainCore extends CompleteLatticeOperator[Parity] with IntOperator[Parity] with Abstraction[Int, Parity] {

  /**
    * Factory method for parities (i.e. abstraction).
    *
    * @param num number that has to be converted to parity
    * @return parity of `n`
    */
  override def alpha(num: Int): Parity =
    if(num % 2 == 0)
      Even
    else
      Odd

  /**
    * @inheritdoc
    */
  def sum(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (ParityTop, _) => ParityTop
      case (_, ParityTop) => ParityTop
      case (a, b) => if(a == b) Even else Odd
    }
  }

  /**
    * @inheritdoc
    */
  def mult(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (Even, _) => Even
      case (_, Even) => Even
      case (ParityTop, _) => ParityTop
      case (_, ParityTop) => ParityTop
      case (Odd, Odd) => Odd
    }
  }

  /**
    * @inheritdoc
    */
  def inverse(p : Parity) : Parity = p

  /**
    * @inheritdoc
    */
  def division(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case _ => ParityTop //Because you cannot know if the result is an integer number. For instance 1/4 is neither even nor odd
    }
  }

  /**
    * @inheritdoc
    */
  def remainder(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (Odd, Odd) => Even
      case (Odd, Even) => Odd
      case (_, _) => ParityTop
      /* Because...
        (_, Even) => Top
        (Even, Odd) => Top
        (ParityTop, Odd) => Top
        (_, ParityTop) => Top
      */
    }
  }

  /**
    * @inheritdoc
    */
  def lub(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityTop, _) => ParityTop
      case (_, ParityTop) => ParityTop
      case (ParityBottom, a) => a
      case (a, ParityBottom) => a
      case (a,b) => if(a == b) a else ParityTop
    }
  }

  /**
    * @inheritdoc
    */
  def glb(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (ParityTop, a) => a
      case (a, ParityTop) => a
      case (a, b) => if(a == b) a else ParityBottom
    }
  }

  /**
    * @inheritdoc
    */
  def compare(p : Parity, q : Parity) : Option[Int] = {
    (p,q) match {
      case (ParityTop, ParityTop) => Option(0)
      case (ParityTop, _) => Option(1)
      case (_, ParityTop) => Option(-1)
      case (ParityBottom, ParityBottom) => Option(0)
      case (ParityBottom, _) => Option(-1)
      case (_, ParityBottom) => Option(1)
      case (a,b) => if(a.equals(b)) Option(0) else Option.empty
    }
  }

  /**
    * @inheritdoc
    */
  override def top: Parity = ParityTop

  /**
    * @inheritdoc
    */
  override def bottom: Parity = ParityBottom

} // end of ParityDomainCore class

object ParityDomainCore {
  def apply() = new ParityDomainCore
} // end of ParityDomainCore companion object
