package it.unipd.jandom.domains.numerical.parity

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}

trait Parity
// multiples of 2
case object Even extends Parity
// multiples of 2 plus 1
case object Odd extends Parity
// no accurate info available for variable
case object ParityTop extends Parity
// no possible value
case object ParityBottom extends Parity

/**
  * Domain with values with the form `a + 2 * Z`.
  * Degenerate case of the Congruence domain (the one with modulus always equal to 2).
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object ParityDomainCore extends CompleteLatticeOperator[Parity] with IntOperator[Parity] with Abstraction[Int, Parity] {

  /**
    * Factory method for parities (i.e. abstraction).
    *
    * @param num number that has to be converted to parity
    * @return parity of `n`
    */
  override def alpha(num: Int): Parity =
    if(num % 2 == 0)
      Even
    else
      Odd

  // stale
  def toParity(n : Double) : Parity = {
    // if (n != n.floor) return ParityTop
    if (n.toInt % 2 == 0)
      Even
    else
      Odd
  }

  /**
    * @inheritdoc
    */
  def sum(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (ParityTop, _) => ParityTop
      case (_, ParityTop) => ParityTop
      case (a, b) => if(a == b) Even else Odd
    }
  }

  /**
    * @inheritdoc
    */
  def mult(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (Even, _) => Even
      case (_, Even) => Even
      case (ParityTop, _) => ParityTop
      case (_, ParityTop) => ParityTop
      case (Odd, Odd) => Odd
    }
  }

  /**
    * @inheritdoc
    */
  def inverse(p : Parity) : Parity = p

  /**
    * @inheritdoc
    */
  def division(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case _ => ParityTop //Because you cannot know if the result is an integer number. For instance 1/4 is neither even nor odd
    }
  }

  /**
    * @inheritdoc
    */
  def remainder(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (Odd, Odd) => Even
      case (_, _) => ParityTop
      /*
        (Even, Odd) => Top
        (ParityTop, Odd) => Top
        (_, ParityTop) => Top
        (_, Even) => Top
      */
    }
  }

  /**
    * @inheritdoc
    */
  def lub(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityTop, _) => ParityTop
      case (_, ParityTop) => ParityTop
      case (ParityBottom, a) => a
      case (a, ParityBottom) => a
      case (a,b) => if(a == b) a else ParityTop
    }
  }

  /**
    * @inheritdoc
    */
  def glb(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (ParityTop, a) => a
      case (a, ParityTop) => a
      case (a, b) => if(a == b) a else ParityBottom
    }
  }

  /**
    * @inheritdoc
    */
  def compare(p : Parity, q : Parity) : Option[Int] = {
    (p,q) match {
      case (ParityTop, ParityTop) => Option(0)
      case (ParityTop, _) => Option(1)
      case (_, ParityTop) => Option(-1)
      case (ParityBottom, ParityBottom) => Option(0)
      case (ParityBottom, _) => Option(-1)
      case (_, ParityBottom) => Option(1)
      case (a,b) => if(a.equals(b)) Option(0) else Option.empty
    }
  }

  /**
    * @inheritdoc
    */
  override def top: Parity = ParityTop

  /**
    * @inheritdoc
    */
  override def bottom: Parity = ParityBottom
} // end ParityDomainCore object
