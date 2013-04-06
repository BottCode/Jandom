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

package it.unich.sci.jandom

import it.unich.sci.jandom.domains.PPLCPolyhedron
import it.unich.sci.jandom.narrowings.DefaultNarrowing
import it.unich.sci.jandom.narrowings.DelayedNarrowingFactory
import it.unich.sci.jandom.ppfactories.MemoizingFactory
import it.unich.sci.jandom.ppfactories.PPFactory.ConstantFactory
import it.unich.sci.jandom.targets.NarrowingStrategy
import it.unich.sci.jandom.targets.WideningScope
import it.unich.sci.jandom.targets.slil.SLILStmt
import it.unich.sci.jandom.widenings.DefaultWidening
import it.unich.sci.jandom.widenings.DelayedWideningFactory
import parma_polyhedra_library.Parma_Polyhedra_Library
import it.unich.sci.jandom.domains.NumericalDomain
import it.unich.sci.jandom.targets.lts.LTS
import it.unich.sci.jandom.widenings.Widening
import it.unich.sci.jandom.narrowings.Narrowing

/**
 * Example program using ''Jandom''.
 * @todo remove from ''Jandom'' and put into a related project.
 */
object JandomExample extends App {

  {
    val source = scala.io.Source.fromFile("examples/nested.R").getLines.mkString("\n")
    val parsed = parsers.RandomParser().parseProgram(source)
    if (parsed.successful) {
      val program = parsed.get
      val params = new targets.Parameters(program) { val domain = domains.BoxDouble }
      params.narrowingStrategy = NarrowingStrategy.Restart
      params.wideningScope = WideningScope.BackEdges
      val ann = program.analyze(params)
      println(program.mkString(ann))
    } else {
      println(parsed)
    }
  }

  {
    val source = scala.io.Source.fromFile("examples/LPinv/berkeley.in").getLines.mkString("\n")
    val parsed = parsers.LPInvParser().parseProgram(source)
    if (parsed.successful) {
      val program = parsed.get
      val params = new targets.Parameters(program) { val domain = PPLCPolyhedron }
      val x= DelayedWideningFactory[LTS](DefaultWidening, 2)
      params.wideningFactory = MemoizingFactory.apply[LTS,Widening](DelayedWideningFactory[LTS](DefaultWidening, 2), program.getAnnotation[Widening])
      params.narrowingFactory = MemoizingFactory.apply[LTS,Narrowing](DelayedNarrowingFactory[LTS](DefaultNarrowing, 2), program.getAnnotation[Narrowing])
      println(program)
      val ann = program.analyze(params)
      println(ann)
    } else {
      println(parsed)
    }
  }
  println("Jandom version: "+version)
  println("PPL version: "+Parma_Polyhedra_Library.version);  
}
