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
      // Top = Mod(Some(1),0)
      case (Mod(Some(1),0), Mod(Some(1),0)) => Option(0)
      case (Mod(Some(1),0), _) => Option(1)
      case (_, Mod(Some(1),0)) => Option(-1)
      case (CongruenceBottom, CongruenceBottom) => Option(0)
      case (CongruenceBottom, _) => Option(-1)
      case (_, CongruenceBottom) => Option(1)
      case (Mod(a0, b0), Mod(a1, b1)) => {
          // c == d
          if ((a0 == a1) && 
              mathematicalOperation.isCongruent(b0,b1,a1) &&
              mathematicalOperation.isCongruent(b1,b0,a0))
            Option(0)
          // c < d (check Miné definition)
          if (mathematicalOperation.isDivisor(a1, a0) && mathematicalOperation.isCongruent(b0, b1, a1))
            Option(-1)
          // c > d
          Option(1)
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

} // end of CongruenceDomainCore

object CongruenceDomainCore {
  def apply() = new CongruenceDomainCore
}