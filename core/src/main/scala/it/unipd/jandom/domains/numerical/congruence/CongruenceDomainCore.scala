package it.unipd.jandom.domains.numerical.congruence

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import Congruence._

/**
  * The Integer Congruence Domain.
  * B# = { (aZ + b) | a ∈ N^* ∪ {0}, b ∈ Z } ∪ {⊥#}}
  * It forms a complete lattice with 
  *   1Z+0 as Top element
  *   ⊥ as Bottom element
  * 
  * Based on: 
  * http://www.dsi.unive.it/~avp/domains.pdf
  * https://www-apr.lip6.fr/~mine/publi/article-mine-sas02.pdf
  */
class CongruenceDomainCore extends CompleteLatticeOperator[Congruence]
  with IntOperator[Congruence] with Abstraction[Int, Congruence]{

  def mathematicalOperation = ExtendedMathematicalOperation()


  /**
    * Reduce the congruence abstract domain, thus making (alpha, C, A, gamma) 
    * a Galois Insertion.
    * aZ+b with a > 0 and a > b
    */
  private def standardForm(c : Congruence) : Congruence = {
    c match {
      case CongruenceBottom => CongruenceBottom
      case Mod(None, _) => c
      case Mod(Some(a), b) => Mod(Some(a), b % a)
    }
  }

  /**
    * @inheritdoc
    */
  override def alpha(num: Int): Congruence =  standardForm(Mod(None, num))
  
  def alpha(a: Option[Int], b: Int): Congruence =  standardForm(Mod(a, b))
  
  /**
    * @inheritdoc
    */
  def sum(c : Congruence, d : Congruence) : Congruence = {
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
         val a = mathematicalOperation.gcd(a0, a1)
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
    */
  def mult(c : Congruence, d : Congruence) : Congruence = {
    (c, d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
        if (a0 == a1 && a0.isEmpty)
          alpha(None, b0 * b1)
        else {
          val a = mathematicalOperation.gcd(mathematicalOperation.*(a0, a1),
            mathematicalOperation.*(a0, Some(b1)), mathematicalOperation.*(a1, Some(b0)))
          val b = b0 * b1
          alpha(a, b)
        }
    }
  }

  /**
    * @inheritdoc
    */
  def division(c: Congruence, d: Congruence): Congruence =
    (c, d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (_, Mod(None,0)) => CongruenceBottom
      case (Mod(a0, b0), Mod(None, b1)) =>
        if(mathematicalOperation.isDivisor(Some(b1),a0) && mathematicalOperation.isDivisor(Some(b1), Some(b0)))
          alpha(mathematicalOperation.division(a0,Some(b1.abs)), b0/b1)
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
      case (Mod(a0, b0), Mod(a1, b1)) =>
        val a = mathematicalOperation.gcd(a0, a1, Some(b1))
        val b = b0
        alpha(a,b)
    }
  }

  /**
    * @inheritdoc
    */
  def leq(c: Congruence, d: Congruence): Option[Boolean] = {
    (c, d) match {
      case (CongruenceBottom, _) => Some(true)
      case (_, CongruenceBottom) => Some(false) //TODO or none?
      case (Mod(a0, b0), Mod(a1, b1)) =>
        if (mathematicalOperation.isDivisor(a1, a0) &&
          mathematicalOperation.isCongruent(b0, b1, a1))
          Some(true)
        else
          None

    }
  }

  /**
    * @inheritdoc
    */
  def compare(c: Congruence, d: Congruence): Option[Int] = {
    val r = leq(c, d)
    val l = leq(d, c)
    (l, r) match {
      case (None, None) => Option.empty
      case (None, Some(a)) =>
        if(a) Option(-1) else Option(1)
        //throw new RuntimeException("It is not possible that a cannot compare to b but b can compare with a" + c + " " + d)
      case (Some(a), None) =>
        if(a) Option(1) else Option(-1)
        //throw new RuntimeException("It is not possible that c cannot compare to d but d can compare with c" + c + " " +d)
      case (Some(true), Some(false)) => Option(1)
      case (Some(true), Some(true)) => Option(0)
      case (Some(false), Some(true)) => Option(-1)
      case (_, _) => throw new RuntimeException("It is not possible that c leq d and d leq c have the same value" + c + " " + d)
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
        if(a0 == a1 && b0 == b1)
          return alpha(a0,b0)
        println("VALUE LUB " + a0 + " " + a1 + " " + b0 + " " + b1)
        val a = mathematicalOperation.gcd(a0, a1, Some((b0-b1).abs))
        println("VALUE LUB " + a)
        val b = Math.min(b0,b1)
        alpha(a,b)
    }
  }

  /**
    * @inheritdoc
    */
  def glb(c : Congruence, d : Congruence) : Congruence = {
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
        if (mathematicalOperation.isCongruent(b0, b1, mathematicalOperation.gcd(a0, a1))) {
          val a = mathematicalOperation.lcm(a0, a1)

          val (_, bezout0, bezout1) = mathematicalOperation.extendedGcd(a0, a1)
          val b = mathematicalOperation.+(
            mathematicalOperation.*(mathematicalOperation.*(a0, bezout1), a),
            mathematicalOperation.*(mathematicalOperation.*(a1, bezout0), a)
          )
          val b2 : Int = b match {
            case None => 0
            case Some(x) => x
          }

          alpha(a, b2)
        } else {
          CongruenceBottom
        }

    }
  }

  /**
    * @inheritdoc
    */
  override def top: Congruence = alpha(Some(1),0)

  /**
    * @inheritdoc
    */
  override def bottom: Congruence = CongruenceBottom

} // end of CongruenceDomainCore

object CongruenceDomainCore {
  def apply() = new CongruenceDomainCore
}