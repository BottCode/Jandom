/**
 * Copyright 2013 Gianluca Amato
 * 
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
 */

package it.unich.sci.jandom.targets

import it.unich.sci.jandom.domains.NumericalProperty
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

/**
 * A mock target only used for test suites. It is private for the jandom package.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
private[jandom] class MockTarget extends Target {
  type Tgt = MockTarget
  type ProgramPoint = Int  
  def getAnnotation[Property] = new HashMap[ProgramPoint,Property] with Annotation[ProgramPoint,Property]
  
  def size = 10
  
  def analyze(params: Parameters): Annotation[ProgramPoint,params.Property] = getAnnotation[params.Property]
}

/** 
 * The companion object for the MockTarget.
 * @author Gianluca Amato <amato@sci.unich.it> 
 */
private[jandom] object MockTarget {
  /**
   * Creates a mock target.
   */
  def apply() = new MockTarget()  
}
