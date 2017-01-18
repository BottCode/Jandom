/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.utils.numberext.RationalExt
import it.unich.scalafix.Box

trait Sign;
case object Plus extends Sign;
case object Minus extends Sign;
case object Zero extends Sign;
case object SignTop extends Sign;
case object SignBottom extends Sign;

/**
 * The most abstract numerical domain. It has a single top element for each dimension.
 * @author Gianluca Amato <gamato@unich.it>
 */
object SignDomain extends NumericalDomain {

  case class Property private[SignDomain] (val dimension: Int, val sign : Array[Sign]) extends NumericalProperty[Property] {
    require(dimension >= 0)

    type Domain = SignDomain.type

    def domain = SignDomain

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
      case other: Property if dimension == other.dimension => Some(0)
      case _ => None
    }
    def widening(that: Property) = this



    def union(that: Property) : Property = {
      require(dimension == that.dimension)
      val newSign = (this.sign, that.sign).zipped.map(
        (s, t) => (s,t) match {
          case (SignTop, _) => SignTop
          case (_, SignTop) => SignTop
          case (SignBottom, a) => a
          case (a, SignBottom) => a
          case (a, b) => if(a == b) a else SignTop
        }
      )
      new Property(dimension, newSign)
    }



    def narrowing(that: Property) = this
    def intersection(that: Property) = this
    def nonDeterministicAssignment(n: Int) = this
    
    
    private def signSum(s: Sign, t: Sign) : Sign = {
      (s,t) match {
        case (SignBottom, _) => SignBottom
        case (_, SignBottom) => SignBottom
        case (_, SignTop) => SignTop
        case (SignTop, _) => SignTop
        case (Plus, Minus) => SignTop
        case (Minus, Plus) => SignTop
        case (Plus, Plus) => Plus
        case (Minus, Minus) => Minus
        case _ => SignTop
      }
    }
    
    private def manOf[T: Manifest](t: T): Manifest[T] = manifest[T]
    
    /**
      * Linear assignment xn := lf
     */
    private def linearEvaluation(lf: LinearForm): Sign = {

      val known = lf.known.toInt;
      println("ciao " + manOf(known) + " READ " + known)
      var ret : Sign = SignTop;
      if(known > 0) 
        ret = Plus
      else if(known < 0)
        ret = Minus
      else
        ret = Zero
      println(ret)
      ret
    }
    
    
    def linearAssignment(n: Int, lf: LinearForm) : Property = {
      val s : Sign = linearEvaluation(lf)
      
      val p =  Property(n, sign.updated(n, s))
      println(sign)
      println("OK")
      this
    }/* {
      new Property(n, sign.updated(n, linearEvaluation(lf)))
    }*/
    def linearInequality(lf: LinearForm) = this
    def linearDisequality(lf: LinearForm) = this

    def minimize(lf: LinearForm) =
      if (lf.homcoeffs.exists(!_.isZero))
        RationalExt.NegativeInfinity
      else
        RationalExt(lf.known)

    def maximize(lf: LinearForm) =
      if (lf.homcoeffs.exists(!_.isZero))
        RationalExt.PositiveInfinity
      else
        RationalExt(lf.known)

    def frequency(lf: LinearForm) =
      if (lf.homcoeffs.exists(!_.isZero))
        Option.empty
      else
        Option(lf.known)

    def constraints = List()

    def isPolyhedral = true

    /**
     * @inheritdoc
     * This is a complete operator for boxes.
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def addVariable: Property =
      if (isEmpty)
        SignDomain.this.bottom(dimension + 1)
      else
        SignDomain.this.top(dimension + 1)
    /**
     * @inheritdoc
     * This is a complete operator for boxes.
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def delVariable(n: Int): Property = {
      require(n < sign.length && n >= 0)
      val newSign = new Array[Sign](dimension - 1)
      Array.copy(sign, 0, newSign, 0, n) 
      Array.copy(sign, n + 1, newSign, n, dimension - n - 1)
      new Property(newSign.length, newSign)
    }

 
    def mapVariables(rho: Seq[Int]) = this

    def isEmpty = false
    def isTop = sign.forall( x =>  x match {
      case SignTop => true
      case _ => false
    })
    def isBottom = sign.forall(x => x match {
      case SignBottom => true
      case _ => false
    })
    def bottom = SignDomain.bottom(sign.length)
    def top = SignDomain.top(sign.length)
    
     /**
     * @inheritdoc
     * @throws $ILLEGAL
     */
    def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (isEmpty)
        "empty"
      else {
        val bounds = for (i <- 0 until dimension) yield {
          val h = sign(i) match {
            case SignTop => "T"
            case SignBottom => "Bottom"
            case Plus => "+"
            case Minus => "-"
            case Zero => "0"
          }
          s"${vars(i)} = ${h}"
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }
  }


  val widenings = Seq(
    WideningDescription("default", "The trivial widening which just returns top.", Box.right[Property]))

  def top(n: Int) = new Property(n, Array.fill(n)(SignTop))
  def bottom(n: Int) = Property(n, Array.fill(n)(SignBottom))
}
