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
package it.unipd.jandom.domains.numerical.utils

/**
  * Implementation of the extended mathematical operations defined in Mine02 using the Option[Int]
  * to simulate the N* U infinite
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object MathLibrary {

  /**
    * Implementation of the extended gcd. Conversion of the pseudo-code exposed in
    * [[http://www.math.unipd.it/~colussi/CompAlgoritmi_2015-16/Primi%20Random.pdf]]
    */
  def extendedGcd(a : Int, b : Int) : (Int, Int, Int) = {
    require(a >= b)
    if(b == 0)
      (a,1,0)
    else{
      val (d,a1,b1) = extendedGcd(b, a % b)
      /* (gcd(x,y), bezout_max, bezout_min) */
      (d, b1, a1 - Math.floorDiv(a,b)*b1)
    }
  }

  def isDivisor(y: Option[Int], y1: Option[Int]): Boolean =
    (y, y1) match {
      case (_, None) => true
      case (None, _) => false
      case (Some(v), Some(w)) => w % v == 0
    }

  def isCongruent(x : Int, x1: Int, y : Option[Int]) : Boolean = {
    if (x == x1)
      true
    else
      isDivisor(y, Some((x-x1).abs))
  }

  private def gcd(a : Int,b : Int) : Int = {
    if(b == 0)
      a
    else
      gcd(b, a % b)
  }

  private def lcm(a : Int, b : Int) : Int = {
    (a * b).abs / gcd(a,b)
  }

  def lcm(y : Option[Int], y1: Option[Int]): Option[Int] = {
    (y, y1) match {
      case (None, a) => None
      case (a, None) => None
      case (Some(a), Some(b)) => Some(lcm(a,b))
    }
  }
  def gcd(y : Option[Int], y1 : Option[Int]) : Option[Int] = {
    (y, y1) match {
      case (None, a) => a
      case (a, None) => a
      case (Some(a), Some(b)) => Some(gcd(a,b))
    }
  }

  def *(y : Option[Int], z : Option[Int]): Option[Int] = {
    (y, z) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(a * b)
    }
  }

  def +(y : Option[Int], z : Option[Int]) : Option[Int] = {
    (y, z) match {
      case (None, _) => z
      case (_, None) => y
      case (Some(a), Some(b)) => Some(a + b)
    }
  }

  def division(y : Option[Int], z : Option[Int]) : Option[Int] = {
    (y, z) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(a/b)
    }
  }
}