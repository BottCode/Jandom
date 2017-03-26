/** Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.ui

import scala.collection.mutable.Buffer
import scala.util.Try
import it.unich.jandom.domains.numerical._
import it.unipd.jandom.domains.numerical._
import it.unipd.jandom.domains.numerical.congruence.CongruenceDomain
import it.unipd.jandom.domains.numerical.parity.ParityDomain
import it.unipd.jandom.domains.numerical.constant.ConstantDomain
import it.unipd.jandom.domains.numerical.mod.ModKDomain
import it.unipd.jandom.domains.numerical.sign.{ESeqDomain, ExtendedSigns01Domain, SignDomain}

/**
 * The ParameterEnumeration for numerical domains.
 */
object NumericalDomains extends ParameterEnumeration[NumericalDomain] {
  val name = "Domain"
  val description = "The numerical domain to use for the analysis."

  val values: Buffer[ParameterValue[NumericalDomain]] = Buffer(
    ParameterValue(SignDomain(), "Sign Domain", "This is a native Scala implementation of the simple sign domain " +
      "(<0, =0, >0)"),
    ParameterValue(ParityDomain(), "Parity Domain", "This is a native Scala implementation of even/odd domain."),
    ParameterValue(ConstantDomain(), "Constant domain", "This is a native Scala implementation of the constant " +
      "propagation domain."),
    ParameterValue(ModKDomain(3), "Module K domain", "This is a native Scala implementation of the module K domain."),
    ParameterValue(ExtendedSigns01Domain(), "ES01 Domain", "This is a native Scala implementation of the sign domain " +
      "extended with the constant 1."),
    ParameterValue(ESeqDomain(), "Extended Sign domain", "This is a native Scala implementation of the sign domain " +
      "extended with >=, <=, !=."),
    ParameterValue(CongruenceDomain(), "Congruence Domain", "This is a native Scala implementation of the domain " +
      "of the congruences with arbitrary modulus."),
    ParameterValue(BoxDoubleDomain(), "BoxDouble", "This is a native Scala implementation of boxes. It is safe " +
      "w.r.t. double arithmetics."),
    ParameterValue(ProductESeqParityDomain(), "ExtendedSign X Parity", "This is a native Scala " +
      "implementation of the reduced product between parity and the extended sign domains."),
    ParameterValue(ProductCongruenceBoxDoubleDomain(), "Congruence X BoxDouble", "This is a native Scala " +
      "implementation of the reduced product between congruence and the interval domain (double)."),
    ParameterValue(FullyReducedProductCongruenceBoxDoubleDomain(), "Congruence X BoxDouble (Fully Reduced) ",
      "This is a native Scala implentation of the fully reduced product between congruence and the interval domain"),
    ParameterValue(SumSignModKDomain(2), "Sign + Parity", "Sum of signs and parity domains."),
    ParameterValue(BoxDoubleDomain(overReals=true), "BoxDouble over Reals", "This is a native Scala implementation of boxes. It is safe " +
      "w.r.t. reals."),
    ParameterValue(ParallelotopeDomain(), "Parallelotope", "This is a native Scala implementation of parallelotopes. It is " +
      "not safe and should not be used."),
    ParameterValue(SumIntParallelotopeDomain(), "BoxDouble + Parallelotope", "Sum of boxes and parallelotopes."),
    ParameterValue(ParallelotopeRationalDomain(), "Parallelotope over Rationals", "This is a native Scala implementation of parallelotopes using rational numbers.")
  )
  val default = values.last

  // Load objects PPLUIInitializer and PPLMacroUIInitializer if available
  Try ( Class.forName ("it.unich.jandom.ui.PPLUIInitializer$") )
  Try ( Class.forName ("it.unich.jandom.ui.PPLMacroUIInitializer$") )
}
