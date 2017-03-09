package it.unipd.jandom.domains.numerical

object ParityDomainCore {
  trait Parity
  case object Even extends Parity
  case object Odd extends Parity
  case object ParityTop extends Parity
  case object ParityBottom extends Parity


  def toParity(t : Int) : Parity =
    if(t % 2 == 0)
      Even
    else
      Odd

  def toParity(n : Double) : Parity = {
    // if (n != n.floor) return ParityTop
    if (n.toInt % 2 == 0)
      Even
    else
      Odd
  }

  def sum(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (ParityTop, _) => ParityTop
      case (_, ParityTop) => ParityTop
      case (a, b) => if(a == b) Even else Odd
    }
  }

  def lub(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityTop, _) => ParityTop
      case (_, ParityTop) => ParityTop
      case (ParityBottom, a) => a
      case (a, ParityBottom) => a
      case (a,b) => if(a == b) a else ParityTop
    }
  }

  def glb(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (ParityTop, a) => a
      case (a, ParityTop) => a
      case (a, b) => if(a == b) a else ParityBottom
    }
  }

  def mult(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case (ParityTop, _) => ParityTop
      case (_, ParityTop) => ParityTop
      case (Even, _) => Even
      case (_, Even) => Even
      case (Odd, Odd) => Odd
    }
  }

  def division(p : Parity, q : Parity) : Parity = {
    (p,q) match {
      case (ParityBottom, _) => ParityBottom
      case (_, ParityBottom) => ParityBottom
      case _ => ParityTop //Because you cannot know if the result is an integer number. For instance 1/4 is neither even nor odd
    }
  }

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

  def inverse(p : Parity) : Parity = {
    p
  }


}