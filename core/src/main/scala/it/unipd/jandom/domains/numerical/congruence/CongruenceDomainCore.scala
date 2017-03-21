package it.unipd.jandom.domains.numerical.congruence

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}

/**
  * Created by mirko on 3/9/17.
  *
  * Based on http://www.dsi.unive.it/~avp/domains.pdf, because we could not retrieve the original
  * paper of Philippe Granger
  */
class CongruenceDomainCore extends CompleteLatticeOperator[Congruence]
  with IntOperator[Congruence] with Abstraction[Int, Congruence]{

  def mathematicalOperation = ExtendedMathematicalOperation()

  /**
    * @inheritdoc
    */
  override def alpha(num: Int): Congruence =  Mod(None , num)

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
        val a = mathematicalOperation.gcd(a0, a1, Some(b1))
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
  def compare(_c: Congruence, _d: Congruence): Option[Int] = {
    val c = standardForm(_c)
    val d = standardForm(_d)
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
        if(a0 == a1 && b0 == b1)
          return Mod(a0, b0)
        println("VALUE LUB " + a0 + " " + a1 + " " + b0 + " " + b1)

        val a = mathematicalOperation.gcd(a0, a1, Some((b0-b1).abs))
        println("VALUE LUB " + a)
        val b = Math.min(b0,b1)
        standardForm(Mod(a, b))

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
        if (mathematicalOperation.isCongruent(b0, b1, mathematicalOperation.gcd(a0, a1))) {
          val a = mathematicalOperation.lcm(a0, a1)

          val (_, bezout0, bezout1) = mathematicalOperation.extendedGcd(a0, a1)
          val b = mathematicalOperation.+(
            mathematicalOperation.*(mathematicalOperation.*(a0, bezout1), a),
            mathematicalOperation.*(mathematicalOperation.*(a1, bezout0), a)
          )
          val b2 = b match {
            case None => 0
            case Some(x) => x
          }

          standardForm(Mod(a, b2))
        } else {
          CongruenceBottom
        }

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
         val a = mathematicalOperation.gcd(a0, a1)
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
          val a = mathematicalOperation.gcd(mathematicalOperation.*(a0, a1),
            mathematicalOperation.*(a0, Some(b1)), mathematicalOperation.*(a1, Some(b0)))
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
      case (Mod(None, a), Mod(None, b)) =>
        if(b == 0)
          CongruenceBottom
        else if(a % b == 0)
          Mod(None, a/b)
        else
          Mod(Some(1), 0)
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
object CongruenceDomainCore {
  def apply() = new CongruenceDomainCore
}