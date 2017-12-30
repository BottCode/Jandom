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
  * Interface for domains which are complete lattices.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
trait CompleteLatticeOperator[AbstractType] {

  /**
    * Returns the least upper bound between two lattice values.
    *
    * @param x first term of the lub
    * @param y second term of the lub
    * @return the lub of `x` and `y`
    */
  def lub(x : AbstractType, y : AbstractType) : AbstractType

  /**
    * Returns the greatest lower bound between two lattice values.
    *
    * @param x first term of the glb
    * @param y second term of the glb
    * @return the glb of `x` and `y`
    */
  def glb(x : AbstractType, y : AbstractType) : AbstractType

  /**
    * Performs a Scala-like comparison (same behaviour as Java's compareTo) 
    * between two lattice values.
    *
    * @param x left hand side
    * @param y right hand side
    * @return 1 if `x` > `y` -- 0 if `x` = `y` -- -1 if `x` < `y`
    */
  def compare(x : AbstractType, y : AbstractType) : Option[Int]

  /**
    * Maximum element of this lattice.
    * @return the top element
    */
  def top : AbstractType

  /**
    * Least element of this lattice.
    * @return the bottom element
    */
  def bottom : AbstractType

}
