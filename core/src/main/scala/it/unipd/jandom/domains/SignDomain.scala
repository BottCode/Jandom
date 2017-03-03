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

package sign.main

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.{LinearForm, NumericalDomain, NumericalProperty}
import it.unich.jandom.utils.numberext.RationalExt
import it.unich.scalafix.Box


/**
  * Sign domain
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
 */
object SignDomain extends NumericalDomain {

  import sign.main.SignFunctions._

  case class Property private[SignDomain] (val sign : Array[Sign]) extends NumericalProperty[Property] {
    val dimension = sign.length
    require(dimension >= 0)

    type Domain = SignDomain.type

    def domain = SignDomain

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
      case other: Property if dimension == other.dimension => Some(0)
      case _ => None
    }
    def widening(that: Property) = this


    /* Compute an upper bound of two abstract properties. */
    def union(that: Property) : Property = {
      println("Union called")
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
      val p = new Property(newSign)
      p
    }



    def narrowing(that: Property) = this


    /**
      * @inheritdoc
      * @note @inheritdoc
      */
     /*Compute an upper approximation of the greatest lower bound of two abstract properties.*/
    def intersection(that: Property) : Property = {
       println("Intersetion called")
       require(dimension == that.dimension)
       val newSign = (this.sign, that.sign).zipped.map(
        (s: Sign, t:Sign) => (s,t) match {
          case (SignTop, a) => a
          case (a, SignTop) => a
          case (_, SignBottom) => SignBottom
          case (SignBottom, _) => SignBottom
          case (a, b) => if(a == b) a else SignBottom
        }
      )


       Property(newSign)
     }


    /**
      * @inheritdoc
      * @note @inheritdoc
      * @throws $ILLEGAL
      */
    def nonDeterministicAssignment(n: Int): Property = Property(sign.updated(n, SignTop))



    /**
      * Compute the minimum and maximum value of a linear form in a box.
      * @param lf a linear form
      * @return a tuple with two components: the first component is the least value, the second component is the greatest value
      * of the linear form over the box.
      */
    private def linearEvaluation(lf: LinearForm): Sign = {
      val known = lf.known.toDouble
      val homcoeffs = lf.homcoeffs.map (_.toDouble).toArray
      linearEvaluation(known, homcoeffs)
    }

    /**
      * Compute the minimum and maximum value of a linear form in a box.
      * @param known the known term of a linear form
      * @return a tuple with two components: the first component is the least value, the second component is the greatest value
      * of the linear form over the box.
      */
    private def linearEvaluation(known: Double, homcoeffs: Array[Double]): Sign = {
      require(homcoeffs.length <= dimension)
      var s: Sign = toSign(known)
      for (i <- homcoeffs.indices) {

        if (homcoeffs(i) > 0) {
          val t: Sign = sign(i)
          s = sum(s, t)
        }
        else if (homcoeffs(i) < 0) {
          val t: Sign = sign(i) match {
            case Minus => Plus
            case Plus => Minus
            case a => a //Zero, Top, Bottom are not changed!
          }
          s = sum(s, t)
        }
      }
      s
    }






    def linearAssignment(n: Int, lf: LinearForm) : Property = {
      require(n < sign.length && n >= 0 && lf.dimension <= dimension)

      val s : Sign = linearEvaluation(lf)
      new Property(sign.updated(n, s))
    }


    /** @inheritdoc
       * @param lf
      */
    def linearInequality(lf: LinearForm) = {
      val s : Sign = linearEvaluation(lf)
      s match {
        case Plus => bottom
        case SignTop => top //IS this the best correct approximation???? //TODO Maybe we can improve this result Case Top????
        case SignBottom => bottom //TODO Check it
        case _ => this
      }
    }

    def linearDisequality(lf: LinearForm) = {
      val s : Sign = linearEvaluation(lf)
      s match {
        case Plus => this
        case Minus => this
        case Zero => bottom
        case SignTop => top //Todo check
        case SignBottom => bottom //TODO check it
      }
    }

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

    def isPolyhedral = false

    /**
     * @inheritdoc
     * Add new variable maintaining the current values
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def addVariable: Property = {
      println(s"Adding variable at the ${dimension} position")
      var newSign = new Array[Sign](sign.length + 1)
      Array.copy(sign, 0, newSign, 0, sign.length)
      newSign(sign.length) = SignTop
      new Property(newSign)
    }
    /**
     * @inheritdoc
     * This is a complete operator for boxes.
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def delVariable(n: Int): Property = {
      require(n < sign.length && n >= 0)
      println(s"Deleting variable at ${n} position")
      val newSign = new Array[Sign](sign.length - 1)
      /*Copy the first n-1 elements */
      Array.copy(sign, 0, newSign, 0, n)
      /* Copy the remaining elements */
      Array.copy(sign, n + 1, newSign, n, sign.length - n - 1)
      new Property(newSign)
    }












    /* Done in complete analogy to the BoxDoubleDomain */
    def mapVariables(rho: Seq[Int]) = {
      require(rho.length == dimension)
      val newdim = rho.count(_ >= 0)
      require(rho forall { i => i >= -1 && i < newdim })
      // we do not check injectivity
      val newSign = new Array[Sign](newdim)

      for ((newi, i) <- rho.zipWithIndex; if newi >= 0) {
        newSign(newi) = sign(i)
      }
      new Property(newSign)
    }

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
            case SignTop => "TOP"
            case SignBottom => "BOTTOM"
            case Plus => "POSITIVE"
            case Minus => "NEGATIVE"
            case Zero => "ZERO"
          }
          s"${vars(i)} = ${h}"
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }



    /**
      * Assignments of the kind `vn = vn * vm`.  The standard implementation determines
      * whether `vn` or `vm` is a constant, and use linearAssignment in such a case.
      * Otherwise, it resorts to a non deterministic assignment.
      * @note $NOTEN
      */
    override def variableMul(n: Int, m: Int) = {
      println("Multiplication")
      sign(n) = mult(sign(n), sign(m));
      this
    }

    override def variableDiv(n : Int, m : Int) = {
      println("Division")
      sign(n) = division(sign(n), sign(m))
      this
    }

     /**
       * @inheritdoc
       * @note @inheritdoc
      */
    override def variableNeg(n: Int = dimension - 1) = {
      sign(n) = inverse(sign(n))
      this
    }


    /**
      * @inheritdoc
      * @note @inheritdoc
      */
    override def variableRem(n: Int = dimension - 2, m: Int = dimension - 1) = {
      sign(n) = remainder(sign(n), sign(m))
      this
    }



  }





  val widenings = Seq(
    WideningDescription("default", "The trivial widening which just returns top.", Box.right[Property]))

  def top(n: Int) = new Property(Array.fill(n)(SignTop))
  def bottom(n: Int) = Property(Array.fill(n)(SignBottom))
}
