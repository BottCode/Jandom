/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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

package it.unich.sci.jandom.domains.objects

/**
 * Class for unordered pairs. It is essentially a set with one or two elements, but
 * we do no implement that interface yet.
 * @param A the type of the elements in the pair.
 */
final class UP[@specialized(Int) A](x: A, y: A)(implicit ordering: Ordering[A]) extends Product2[A, A] {

  def canEqual(that: Any) = that.isInstanceOf[UP[A]]

  val _1 = ordering.min(x, y)
  val _2 = ordering.max(x, y)

  def replace(newval: A, oldval: A): UP[A] =
    UP(if (_1 == oldval) newval else _1, if (_2 == oldval) newval else _2)

  def replace(f: A => A): UP[A] = UP(f(_1),f(_2))

  def contains(v: A) = _1 == v || _2 == v

  override def equals(that: Any) = that match {
    case that: UP[A] => _1 == that._1 && _2 == that._2
    case _ => false
  }
  override def hashCode = _1.hashCode * 41 + _2.hashCode
  override def toString = (_1, _2).toString
}

object UP {
  def apply[A: Ordering](x: A, y: A) = new UP(x, y)
  def apply[A: Ordering](p: (A, A)) = new UP(p._1, p._2)
  def unapply[A](up: UP[A]): Option[(A, A)] = Some((up._1, up._2))
}
