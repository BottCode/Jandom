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
package it.unipd.jandom.domains.numerical.congruence

import it.unipd.jandom.domains.numerical.utils.{MathLibrary => M}
import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import Congruence._

/**
  * The Integer Congruence Domain.
  * B# = { (aZ + b) | a ∈ N* ∪ {∞}, b ∈ Z } ∪ {⊥#}}
  * It forms a complete lattice with
  *   1Z+0 as Top element
  *   ⊥ as Bottom element
  *
  * Based on:
  * [[http://www.dsi.unive.it/~avp/domains.pdf]]
  * [[https://www-apr.lip6.fr/~mine/publi/article-mine-sas02.pdf]]
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class CongruenceDomainCore extends CompleteLatticeOperator[Congruence]
  with IntOperator[Congruence] with Abstraction[Int, Congruence]{

  /**
    * @inheritdoc
    */
  override def alpha(num: Int): Congruence =  standardForm(Mod(None, num))

  /**
    * overloaded abstraction function: 
    * it is necessary to convert all the aZ+b formS 
    * in standard forms a'Z+b' with a' > 0 and 0 <= b' < a'.
    * All the methods of this class have to use alpha 
    * to construct abstract elements of this domain in the standard form.
    */
  def alpha(a: Option[Int], b: Int): Congruence =  standardForm(Mod(a, b))

  /**
    * @inheritdoc
    */
  def sum(c : Congruence, d : Congruence) : Congruence = {
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
        val a = M.gcd(a0, a1)
        val b = b0 + b1
        alpha(a,b)
    }
  }

  /**
    * @inheritdoc
    */
  def inverse(c : Congruence) : Congruence = {
    c match {
      case CongruenceBottom => CongruenceBottom
      case Mod(a, b) => alpha(a, -b)
    }
  }

  /**
    * @inheritdoc
    * @note 0Z+0 which Miné represents as Inf*Z+0 denotes the singleton {0}
    *       We need to distinguish the cases with 0Z+0 from the others 
    *       to avoid computing gcd(0,0) which will result in a runtime error
    */
  def mult(c : Congruence, d : Congruence) : Congruence = {
    (c, d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(None, 0), _) => Mod(None, 0) //{0} * something = {0}
      case (_, Mod(None, 0)) => Mod(None, 0) //something * {0} = {0}
      case (Mod(None, c0), Mod(None, c1)) => Mod(None, c0 * c1) //Multiplication between constants
      case (Mod(a0, b0), Mod(a1, b1)) =>
        val a = M.gcd(M.*(a0, Some(b1)),
          M.gcd(M.*(a0, a1), M.*(a1, Some(b0))))
        val b = b0 * b1
        alpha(a, b)
    }
  }

  /**
    * @inheritdoc
    */
  def division(c: Congruence, d: Congruence): Congruence =
    (c, d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (_, Mod(None,0)) => CongruenceBottom //Division by zero
      case (Mod(a0, b0), Mod(None, b1)) =>
        if(M.isDivisor(Some(b1),a0) && M.isDivisor(Some(b1), Some(b0)))
          alpha(M.division(a0,Some(b1.abs)), b0/b1)
        else
          alpha(Some(1), 0) //top element
      case (_, _) => alpha(Some(1), 0) //top element
    }

  /**
    * @inheritdoc
    */
  def remainder(c: Congruence, d: Congruence): Congruence = {
    (c, d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (_, Mod(None, 0)) => CongruenceBottom //Division by zero
      case (Mod(a0, b0), Mod(a1, b1)) =>
        val a = M.gcd(a0,M.gcd(a1, Some(b1)))
        val b = b0
        alpha(a,b)
    }
  }

  /**
    * @inheritdoc
    */
  def lub(c : Congruence, d : Congruence) : Congruence = {
    (c,d) match {
      case (CongruenceBottom, _) => d
      case (_, CongruenceBottom) => c
      case (Mod(a0, b0), Mod(a1,b1)) =>
        if(a0 == a1 && b0 == b1) //if c and d are equal
          return alpha(a0,b0)
        val btmp = (b0 - b1).abs match {
          case 0 => None
          case x => Some(x)
        }
        val a = M.gcd(a0, M.gcd(a1, btmp))
        val b = Math.min(b0,b1)
        alpha(a,b)
    }
  }

  /**
    * @inheritdoc
    * @note (a0Z + b0) ∩ (a1Z + b1)=
    *  {gcd(a0,a1)Z+b2  if b0 ≡ b1 lcm(a0,a1) //[caseA]
    *  {⊥               otherwise
    * how to compute b2 must be valid ([caseA] holds):
    *
    *   if (a0,a1) are coprimes then b2 is computed using the
    *   Bezout theorem and the Chinese remainder theorem solving the system
    *   { b2 ≡ b0 mod a0
    *   { b2 ≡ b1 mod a1
    *
    *   if (a0,a1) are NOT coprimes, then there is an intersection between the
    *   two congruence only if the constants are equal (b0==b1). So b2=b0. 
    */
  def glb(c : Congruence, d : Congruence) : Congruence = {
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      // Top = Mod(Some(1),0)
      case (Mod(Some(1),0), _) => d
      case (_,Mod(Some(1),0)) => c
      case (Mod(None,x), Mod(None,y)) => if(x == y) c else CongruenceBottom
      case (Mod(None,x), Mod(a0, b0)) => if(M.isCongruent(x, b0, a0)) c else CongruenceBottom
      case (Mod(a0, b0), Mod(None,x)) => if(M.isCongruent(x, b0, a0)) d else CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
        if (M.isCongruent(b0, b1, M.gcd(a0, a1))) {
          val a = M.lcm(a0, a1)
          /* coprime test - the chinese remainder theorem can be applied */
          if(M.gcd(a0,a1).contains(1)){
            var (x0, x1, y0, y1) = (a0.get, a1.get, b0, b1)

            if(Math.max(a0.get,a1.get) == a0.get){ 
              /*
              Swap values in order to have only one case to distinguish:
              x1 contains the maximum and x0 the minimum, 
              swap the b1,b0 accordingly
              */
              x1=a0.get
              x0=a1.get
              y0=b1
              y1=b0
            }

            val (_, bezout0, bezout1)= M.extendedGcd(x1,x0)
            val x= y0*bezout0*x1+y1*bezout1*x0
            alpha(a,x)
          }
          /* !coprime(a0,a1) case */
          else if(b0 == b1)
            alpha(a, b0)
          else
            CongruenceBottom
        }
        else
          CongruenceBottom
    }
  }

  /**
    * @inheritdoc
    */
  def compare(c: Congruence, d: Congruence): Option[Int] = {
    (c, d) match {

      case (CongruenceBottom, CongruenceBottom) => Option(0)
      case (CongruenceBottom, _) => Option(-1)
      case (_, CongruenceBottom) => Option(1)

      case (Mod(a0, b0), Mod(a1, b1)) =>
        var cleqd = false
        var dleqc = false
        if(M.isDivisor(a1, a0) && M.isCongruent(b0,b1,a1))
          cleqd = true
        if(M.isDivisor(a0,a1) && M.isCongruent(b1, b0, a0))
          dleqc = true

        if(cleqd && dleqc)
          Option(0)
        else if(cleqd)
          Option(-1)
        else if(dleqc)
          Option(+1)
        else
          Option.empty
    }
  }

  /**
    * @inheritdoc
    * @note it is necessary to use alpha because it generates abstract elements
    * in the standard form
    */
  override def top: Congruence = alpha(Some(1),0)

  /**
    * @inheritdoc
    */
  override def bottom: Congruence = CongruenceBottom

  /**
    * Reduce the congruence abstract domain by removing redundant elements.
    * All the congruences are converted in the form:
    * aZ+b with a > 0 and 0<= b < a
    */
  private def standardForm(c : Congruence) : Congruence = {
    c match {
      case CongruenceBottom => CongruenceBottom
      case Mod(None, _) => c
      case Mod(Some(a), b) => Mod(Some(a), ((b % a) + a) % a)
    }
  }

} // end of CongruenceDomainCore class

object CongruenceDomainCore {
  def apply() = new CongruenceDomainCore
} // end of CongruenceDomainCore companion object
