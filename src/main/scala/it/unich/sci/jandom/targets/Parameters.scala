/**
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 *
 * (c) 2012 Gianluca Amato
 */

package it.unich.sci.jandom
package targets
  
import domains.{NumericalProperty,NumericalDomain}
import widenings.{Widening, DefaultWidening}
import narrowings.{Narrowing, DefaultNarrowing}
import ppfactories.PPFactory

/**
 * This class is used to keep parameters for analyzers.
 * @tparam Property the type of property described by the analysis
 * @param val the numerical domain for the analysis
 * @param tgt the target for the analysis
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class Parameters[Property <: NumericalProperty[Property], Tgt <: Target] (val domain: NumericalDomain[Property], val tgt: Tgt) {  
  /**
  * The widening factory used in the analysis. Defaults to the factory for the standard domain widening.
  */
  var wideningFactory: PPFactory[Tgt, Widening] = DefaultWidening
  
  /**
   * The narrowing factory used in the analysis. Defaults to the standard domain narrowing. 
   */
  var narrowingFactory: PPFactory[Tgt,Narrowing] = DefaultNarrowing
  
  /**
   * This parameter determines whether results are saved for each program point or only for widening points.
   */
  var allPPResult = true
  
  /**
   * This objects determines the scope for widenings. The available alternatives are:
   * - Output: standard application of widening at the exit of join nodes
   * - BackEdges: widening is applied at the entrance of join nodes, but only on back edges
   * - Random: the scope used on Random. Widening is applied at the exit of join nodes, but join is only
   *           applied once.
   */
  object WideningScope extends Enumeration {
    type WideningScope = Value
    val Output, BackEdges, Random = Value
  }
  
  /**
   * This parameter determines whether standard or local widening is used. At the moment, this is only supported
   * by the SLSL target.
   */
  var wideningScope = WideningScope.Output
  
  /**
   * This ojects determines the strategy used for narrowings. The available alternatives are:
   * - None: no narrowing is performed
   * - Separate: first only widenings are perfomed, then all narrowings
   * - Restart: the standard Random strategy of perfoming Narrowing intertwined with Widening
   * - Continue: similar to Restart, but during narrowing of outer loops, inner loops only performs narrowing
   * At the moment, this is only supported by the SLIL target.
   */
  object NarrowingStrategy extends Enumeration {
    type NarrowingStrategy = Value
    val None, Separate, Restart, Continue = Value
  }
  
  /**
   * This parameter determine the interlacing strategy between narrowing and widening
   */
  var narrowingStrategy = NarrowingStrategy.Restart
  
}
