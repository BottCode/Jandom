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
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
 */
object SignDomain extends NumericalDomain {

  import sign.main.SignFunctions._

  case class Property private[SignDomain] (val sign : Array[Sign], val unreachable: Boolean) extends NumericalProperty[Property] {
    val dimension = sign.length
    require(dimension >= 0)

    type Domain = SignDomain.type

    def domain = SignDomain

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
      case other: Property if dimension == other.dimension => Some(0)
      case _ => None
    }

    //TODO: Add widening implementation
    def widening(that: Property) = {
      println("Widening called")
      println(s"This: $this")
      println(s"That: $that")
      println("Widening: " + this)
      this
    }


    /* Compute an upper bound of two abstract properties. */
    def union(that: Property) : Property = {
      println("Union called")
      require(dimension == that.dimension)
      val newSign = (this.sign, that.sign).zipped.map( lub(_, _) )
      println(s"This: $this")
      println(s"That: $that")
      println("Lub: " + new Property(newSign, unreachable && that.unreachable))
      new Property(newSign, unreachable && that.unreachable)
    }


    //TODO: Add narrowing implementation
    def narrowing(that: Property) = {
      println("Narrowing called")
      println(s"This: $this")
      println(s"That: $that")
      println("Narrowing: " + this)
      this
    }


    /**
      * @inheritdoc
      * @note @inheritdoc
      */
     /*Compute an upper approximation of the greatest lower bound of two abstract properties.*/
    def intersection(that: Property) : Property = {
      println("Intersection called")
      require(dimension == that.dimension)
      val newSign = (this.sign, that.sign).zipped.map( glb(_, _) )
      println(s"This: $this")
      println(s"That: $that")
      println("Glb: " + SignDomain.this(newSign))
      SignDomain.this(newSign)
     }


    /**
      * @inheritdoc
      * @note @inheritdoc
      * @throws $ILLEGAL
      */
    def nonDeterministicAssignment(n: Int): Property = {
      if(unreachable)
        return this
      new Property(sign.updated(n, SignTop), false)
    }



    /**
      * Compute the minimum and maximum value of a linear form in a box.
      *
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
      *
      * @param known the known term of a linear form
      * @return a tuple with two components: the first component is the least value, the second component is the greatest value
      * of the linear form over the box.
      */
    private def linearEvaluation(known: Double, homcoeffs: Array[Double]): Sign = {
      require(homcoeffs.length <= dimension)
      var s: Sign = toSign(known)
      if (unreachable && homcoeffs.exists { _ != 0 }) return SignTop
      for (i <- homcoeffs.indices) {
        if (homcoeffs(i) > 0) {
          val t: Sign = sign(i)
          s = sum(s, t)
        }
        else if (homcoeffs(i) < 0) {
          val t: Sign = inverse(sign(i))
          s = sum(s, t)
        }
      }
      s
    }

    def linearAssignment(pos: Int, lf: LinearForm) : Property = {
      require(pos < sign.length && pos >= 0 && lf.dimension <= dimension)
      println(s"Assigning... $this \nwith linear form: $lf \nand number n: $pos")
      if(unreachable)
        return this
      val s : Sign = linearEvaluation(lf)
      println("Generated: " + new Property(sign.updated(pos, s), false))
      new Property(sign.updated(pos, s), false)
    }


    /** @inheritdoc
       * @param lf
      */
    def linearInequality(lf: LinearForm) : Property = {
      val s : Sign = linearEvaluation(lf)
      if (isEmpty)
        return this
      s match {
        case Plus => bottom
        case SignTop => top //IS this the best correct approximation???? //TODO Maybe we can improve this result Case Top????
        case SignBottom => bottom //TODO Check it
        case _ => this
      }
    }

    /**
      * @inheritdoc
      * @param lf
      */
    def linearDisequality(lf: LinearForm) : Property = {
      if (isEmpty)
        return this
      val s : Sign = linearEvaluation(lf)
      s match {
        case Plus => this
        case Minus => this
        case Zero => bottom
        case SignTop => top //lub(Plus, Minus)
        case SignBottom => bottom
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
      if (unreachable)
        return SignDomain.this.bottom(dimension + 1)
      SignDomain.this(sign :+ SignTop)
    }

    /**
     * @inheritdoc
     * This is a complete operator for boxes.
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def delVariable(pos: Int): Property = {
      require(pos < sign.length && pos >= 0)
      println(s"Deleting variable at ${pos} position")
      println(s"This: $this")
      /*if(sign.init.forall(s => s.equals(SignBottom)))
        return new Property(Array.fill[Sign](sign.length - 1)(SignTop), false)*/
      val newSign = new Array[Sign](sign.length - 1)
      // Copy the first pos-1 elements
      Array.copy(sign, 0, newSign, 0, pos)
      // Copy the remaining elements
      Array.copy(sign, pos + 1, newSign, pos, sign.length - pos - 1)
      new Property(newSign, unreachable)
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
      new Property(newSign, unreachable)
    }

    def isEmpty = unreachable

    def isTop = !isEmpty && sign.forall(s => s.equals(SignTop))
    def isBottom = isEmpty

    def bottom = SignDomain.bottom(sign.length)
    def top = SignDomain.top(sign.length)

     /**
     * @inheritdoc
     * @throws $ILLEGAL
     */
    def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (unreachable)
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
      *
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

  def apply(signsArray : Array[Sign]): Property = {
    new Property(signsArray, signsArray.forall(s => s.equals(SignBottom)))
  }

  val widenings = Seq(
    WideningDescription("default", "The trivial widening which just returns top.", Box.right[Property]))

  def top(n: Int) = new Property(Array.fill(n)(SignTop), false)
  def bottom(n: Int) = new Property(Array.fill(n)(SignBottom), true)
}
