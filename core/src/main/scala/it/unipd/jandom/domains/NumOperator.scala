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
package it.unipd.jandom.domains

/**
  * Interface for operators with basic algebraic operations.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
trait NumOperator[AbstractType]{

  /**
    * Performs the (-) prefix operation.
    *
    * @param x variable that will be inverted
    * @return the inverse of `x`
    */
  def inverse(x : AbstractType) : AbstractType

  /**
    * Returns the sum of two domain variables.
    *
    * @param x the first addend
    * @param y the second addend
    * @return the result of the addition
    */
  def sum(x : AbstractType, y : AbstractType) : AbstractType

  /**
    * Returns the multiplication of two domain variables.
    *
    * @param x the first factor
    * @param y the second factor
    * @return the product
    */
  def mult(x : AbstractType, y : AbstractType) : AbstractType

  /**
    * Returns the division of two domain variables.
    *
    * @param x the dividend
    * @param y the divisor
    * @return the result of the division
    */
  def division(x : AbstractType, y : AbstractType) : AbstractType

}
