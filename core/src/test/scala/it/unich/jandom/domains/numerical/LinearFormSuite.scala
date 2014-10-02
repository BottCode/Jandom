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
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains.numerical

import org.scalatest.FunSuite

/**
 * The test suite for the LinearForm class
 * @author Gianluca Amato <gamato@unich.it>
 */

class LinearFormSuite extends FunSuite {

  test("Variable constructor") {
    var lf = LinearForm.v[Int](1)
    assertResult(LinearForm(0, 0, 1)) { lf }
  }

  test("Sparse constructor") {
    val lf1 = LinearForm(0, 0, 2)
    val lf2 = LinearForm(0, 1 -> 2)
    assertResult(lf1) { lf2 }
  }

  test("Accessors") {
    val lf = LinearForm(1, 2, -1)
    assertResult(1) { lf.known }
    assertResult(Seq(1, 2, -1)) { lf.coeffs }
    assertResult(Seq(2, -1)) { lf.homcoeffs }
    assertResult(2) { lf.dimension }
    assertResult(Seq(0 -> 2, 1 -> -1)) { lf.pairs }
  }

  test("Arithmetic operations") {
    val lf1 = LinearForm(1, 2, -1)
    val lf2 = LinearForm(1, 0, 3)
    val lf3 = LinearForm(2, 2, 2)
    assertResult(LinearForm(-1, -2, 1)) { -lf1 }
    assertResult(LinearForm(2, 2, 2)) { lf1 + lf2 }
    assertResult(LinearForm(0, 2, -4)) { lf1 - lf2 }
    assertResult(LinearForm(2, 4, -2)) { lf1 * 2 }
    assertResult(Some(LinearForm(2, 4, -2))) { 2 * lf1 }
    assertResult(None) { lf1 * lf2 }
    assertResult(LinearForm(0.5, 1, -0.5)) { lf1.toDouble / 2 }
    assertResult(Some(LinearForm(0.5, 1, -0.5))) { lf1.toDouble / LinearForm(2.0) }
    assertResult(None) { lf1.toDouble / lf2.toDouble }
    intercept[ClassCastException] { lf1 / 2 }
    intercept[ClassCastException] { lf1 / lf2 }
  }

  test("Equality") {
    val lf1 = LinearForm(1, 0, 3)
    val lf2 = LinearForm(1, 0, 3)
    assertResult(lf1) { lf2 }
    val lf3 = LinearForm(1, 0, 1)
    assertResult(false)(lf1 == lf3)
  }

  test("Conversion to String") {
    val lf = LinearForm(1, 2, -1)
    assertResult("1+2*v0-v1") { lf.toString }
    val lf2 = LinearForm(1, 0, 3)
    assertResult("1+3*v1") { lf2.toString }
  }

  test("isZero and isConstant") {
    val lf = LinearForm(2.0)
    assert(lf.isConstant)
    assert(!lf.isZero)

    val lf2 = LinearForm(2.0, 0.0, 0.0)
    assert(lf2.isConstant)
    assert(!lf2.isZero)

    val lf3 = LinearForm(0.0)
    assert(lf3.isConstant)
    assert(lf3.isZero)

    val lf4 = LinearForm(0.0, 0.0, 0.0)
    assert(lf4.isConstant)
    assert(lf4.isZero)

    val lf5 = LinearForm(0.0, 1.0)
    assert(! lf5.isConstant)
    assert(! lf5.isZero)
  }
}
