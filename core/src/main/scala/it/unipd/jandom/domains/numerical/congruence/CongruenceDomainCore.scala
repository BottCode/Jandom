package it.unipd.jandom.domains.numerical.congruence

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}

trait Congruence
case class Mod(a : Option[Int], b : Int) extends Congruence {
  require(a match { // Checking if the value is bigger than 0 as in Mine02
    case None => true
    case Some(x) => x > 0
  })
}
case object CongruenceBottom extends Congruence //Corresponds to the empty set

/**
  * Created by mirko on 3/9/17.
  *
  * Based on http://www.dsi.unive.it/~avp/domains.pdf, because we could not retrieve the original
  * paper of Philippe Granger
  */
object CongruenceDomainCore extends CompleteLatticeOperator[Congruence]
  with IntOperator[Congruence] with Abstraction[Int, Congruence]{

  /**
    * @inheritdoc
    */
  override def alpha(num: Int): Congruence =  Mod(Some(0), num)

  /**
    * @inheritdoc
    */
  def remainder(_c: Congruence, _d: Congruence): Congruence = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c, d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
        val a = ExtendedMathematicalOperation.gcd(a0, a1, Some(b1))
        val b = b0
        Mod(a, b)
    }
  }

  /**
    * @inheritdoc
    */
  def leq(_c: Congruence, _d: Congruence): Option[Boolean] = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c, d) match {
      case (CongruenceBottom, _) => Some(true)
      case (_, CongruenceBottom) => Some(false)
      case (Mod(a0, b0), Mod(a1, b1)) =>
        if (ExtendedMathematicalOperation.isDivisor(a1, a0) && ExtendedMathematicalOperation.isCongruent(b0, b1, ExtendedMathematicalOperation.gcd(a0, a1)))
          Some(true)
        else
          None

    }
  }

  /**
    * @inheritdoc
    */
  def compare(_c: Congruence, _d: Congruence): Option[Int] = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    val r = leq(c, d)
    val l = leq(d, c)
    (l, r) match {
      case (None, None) => Option.empty
      case (None, _) => throw new RuntimeException("It is not possible that a cannot compare to b but b can compare with a")
      case (_, None) => throw new RuntimeException("It is not possible that c cannot compare to d but d can compare with c")
      case (Some(true), Some(false)) => Option(1)
      case (Some(true), Some(true)) => Option(0)
      case (Some(false), Some(true)) => Option(-1)
      case (_, _) => throw new RuntimeException("It is not possible that c leq d and d leq c have the same value")
    }
  }

  /**
    * @note reduce the congruence abstract domain, thus making (alpha, C, A, gamma) a GI
    */
  def standardForm(c : Congruence) : Congruence = {
    c match {
      case CongruenceBottom => CongruenceBottom
      case Mod(None, _) => c
      case Mod(Some(a), b) => Mod(Some(a), b % a)
    }
  }

  /**
    * @inheritdoc
    */
  def lub(_c : Congruence, _d : Congruence) : Congruence = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c,d) match {
      case (CongruenceBottom, a) => a
      case (a, CongruenceBottom) => a
      case (Mod(a0, b0), Mod(a1,b1)) =>
        val a = ExtendedMathematicalOperation.gcd(a0, a1, Some((b0-b1).abs))
        val b = Math.min(b0,b1)
        Mod(a, b)
    }
  }

  /**
    * @inheritdoc
    */
  def glb(_c : Congruence, _d : Congruence) : Congruence = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
       if(ExtendedMathematicalOperation.isCongruent(b0, b1, ExtendedMathematicalOperation.gcd(a0,a1))){
         val a = ExtendedMathematicalOperation.gcd(a0, a1)
         val bOpt = ExtendedMathematicalOperation.lcm(a0, a1)  // Bezout theorem
         bOpt match {
           case Some(lcmA0A1) =>
             val b = b0 % lcmA0A1                              // Bezout theorem
             Mod(a, b)
           case _ => bottom
         }
       } else
         CongruenceBottom
    }
  }

  /**
    * @inheritdoc
    */
  def sum(_c : Congruence, _d : Congruence) : Congruence = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
         val a = ExtendedMathematicalOperation.gcd(a0, a1)
         val b = b0 + b1
         standardForm(Mod(a, b))
    }
  }

  /**
    * @inheritdoc
    */
  def inverse(c : Congruence) : Congruence = {
    c match {
      case CongruenceBottom => CongruenceBottom
      case Mod(a, b) =>
       Mod(a, -b)
    }
  }

  /**
    * @inheritdoc
    */
  def mult(_c : Congruence, _d : Congruence) : Congruence = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c, d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
        if (a0 == a1 && a0.isEmpty) {
          Mod(None, b0 * b1)
        }
        else {
          val a = ExtendedMathematicalOperation.gcd(ExtendedMathematicalOperation.*(a0, a1),
            ExtendedMathematicalOperation.*(a0, Some(b1)), ExtendedMathematicalOperation.*(a1, Some(b0)))
          val b = b0 * b1
          Mod(a, b)
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
      case (_, _) => Mod(Some(1), 0) //top element
    }


  /**
    * @inheritdoc
    */
  override def top: Congruence = Mod(Some(1),0)

  /**
    * @inheritdoc
    */
  override def bottom: Congruence = CongruenceBottom

  def getString(c: Congruence): String = {
    c match {
      case CongruenceBottom => "Empty Set"
      case Mod(a, b) =>
        var s: String = ""
        if (!(a.isEmpty)) {
          if (a != Option(1))
            s += a
          s += "Z"
          if (b != 0) {
            s += b
          }
        } else {
          s += b
        }
        s
    }
  }
} // end of CongruenceDomainCore
