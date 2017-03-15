package it.unipd.jandom.domains.numerical.congruence

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}


trait Congruence
case class Mod(a : Int, b : Int) extends Congruence
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
    * @note reduce the congruence abstract domain, thus making (alpha, C, A, gamma) a GI
    */
  def standardForm(c : Congruence) : Congruence = {
    c match {
      case CongruenceBottom => CongruenceBottom
      case Mod(0,_) => c
      case Mod(a,b) => Mod(a, b % a)
    }
  }

  /**
    * Greatest common divisor implementation taken from Rosetta https://rosettacode.org/wiki/Greatest_common_divisor
    */
  private def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
  /* Extension of gcd with 3 inputs */
  private def gcd(a : Int, b: Int, c : Int) : Int = gcd(a, gcd(b,c))

  def lub(_c : Congruence, _d : Congruence) : Congruence = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c,d) match {
      case (CongruenceBottom, _) => c
      case (_, CongruenceBottom) => d
      case (Mod(a0, b0), Mod(a1,b1)) =>
        if(compare(c,d) == Option(0))
          c
        else {
          val a = gcd(a0, a1, (b0 - b1).abs)
          val b = Math.min(b0, b1)
          standardForm(Mod(a, b))
        }
    }
  }

  /**
    * Least common divisor implementation taken from Rosetta https://rosettacode.org/wiki/Least_common_multiple
    */
  private def lcm(a: Int, b: Int) : Int =(a*b).abs/gcd(a,b)


  def glb(_c : Congruence, _d : Congruence) :Congruence = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
        if(compare(c,d) == Option(0))
          c
        else if(b0 == b1 % gcd(a0,a1)) {
          val a = lcm(a0, a1)
          val b = b1 % a1
          standardForm(Mod(a,b))
        }
        else {
          CongruenceBottom
        }
    }
  }

  def sum(_c : Congruence, _d : Congruence) : Congruence = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
        if (a0 == a1 && a0 == 0) //c and d are constants
          Mod(0, b0 + b1)
        else {
          val a = gcd(a0, a1)
          val b = Math.min(b0, b1)
          Mod(a, b)
        }
    }
  }

  def inverse(c : Congruence) : Congruence = {
    c match {
      case CongruenceBottom => CongruenceBottom
      case Mod(a, b) =>
        Mod(a, -b)
    }
  }


  def mult(_c : Congruence, _d : Congruence) : Congruence = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
        if (a0 == a1 && a0 == 0) //c and d are constants!
          Mod(0, b0 * b1)
        else {
          val a = gcd(a0 * a1, a0 * b1, a1 * b0)
          val b = b0 * b1
          Mod(a, b)
        }
    }
  }

  def division(c : Congruence, d : Congruence) : Congruence = {
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (_, _) => Mod(1, 0) //top element
    }
  }

  def remainder(_c : Congruence, _d : Congruence) : Congruence = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c,d) match {
      case (CongruenceBottom, _) => CongruenceBottom
      case (_, CongruenceBottom) => CongruenceBottom
      case (Mod(a0, b0), Mod(a1, b1)) =>
        val a = gcd(a0,a1,b1)
        val b = b0
        Mod(a,b)
    }
  }

  def getString(c : Congruence) : String = {
    c match {
      case CongruenceBottom => "Empty Set"
      case Mod(a, b) =>
        var s : String = "";
        if(a != 0) {
          if(a != 1)
            s += a
          s += "Z"
          if(b != 0){
            s += b
          }
        } else {
          s += b
        }
        s
    }
  }

  private def leq(_c : Congruence, _d : Congruence) : Option[Boolean] = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    (c, d) match {
      case (CongruenceBottom, _) => Some(true)
      case (_, CongruenceBottom) => Some(false)
      case (Mod(a0, b0), Mod(a1, b1)) =>
        if(a1 == a0 && a1 == 0)
          Some(b0 <= b1)
        else if(a0 == 0 || a1 == 0)
          None
        else if (a1 % a0 == 0 && b0 == b1 % a1)
          Some(true)
        else
          None
    }
  }


  def compare(_c : Congruence, _d : Congruence) : Option[Int] = {
    val c = standardForm(_c)
    val d = standardForm(_d)
    val r = leq(c, d)
    val l = leq(d, c)
    (l,r) match {
      case (None, None) => Option.empty
      case (None, _) => throw new RuntimeException("It is not possible that a cannot compare to b but b can compare with a")
      case (_, None) => throw new RuntimeException("It is not possible that c cannot compare to d but d can compare with c")
      case (Some(true), Some(false)) => Option(1)
      case (Some(true), Some(true)) => Option(0)
      case (Some(false), Some(true)) => Option(-1)
      case (_, _) => throw new RuntimeException("It is not possible that c leq d and d leq c have the same value")
    }


  }

  override def top: Congruence = Mod(1,0)

  override def bottom: Congruence = CongruenceBottom

  override def alpha(num: Int): Congruence =  Mod(0, num)
}
