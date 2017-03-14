package it.unipd.jandom.domains.numerical

import it.unipd.jandom.domains._

/**
  *
  *
  * @author $assume
  */
trait Mod3
case class RestClass(num : Int) extends Mod3
case object Mod3Bottom extends Mod3
case object Mod3Top extends Mod3

class Mod3DomainCore extends CompleteLatticeOperator[Mod3] with IntOperator[Mod3] {

  def toMod3(t : Int) : Mod3 = RestClass(t % 3)

  def sum(m : Mod3, n : Mod3) : Mod3 = {
    (m, n) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (Mod3Top, _) => Mod3Top
      case (_, Mod3Top) => Mod3Top
      case (RestClass(m1), RestClass(n1)) =>
        toMod3(m1+n1)
    }
  }

  def lub(m : Mod3, n : Mod3) : Mod3 = {
    (m,n) match {
      case (Mod3Top, _) => Mod3Top
      case (_, Mod3Top) => Mod3Top
      case (Mod3Bottom, _) => n
      case (_, Mod3Bottom) => m
      case (_, _) => if(m == n) m else Mod3Top
    }
  }

  def glb(m : Mod3, n : Mod3) : Mod3 = {
    (m, n) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (Mod3Top, _) => n
      case (_, Mod3Top) => m
      case (_, _) => if(m == n) m else Mod3Bottom
    }
  }

  def mult(m : Mod3, n : Mod3) : Mod3 = {
    (m, n) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (RestClass(0), _) => RestClass(0)
      case (_, RestClass(0)) => RestClass(0)
      case (Mod3Top, _) => Mod3Top
      case (_, Mod3Top) => Mod3Top
      case (RestClass(c1), RestClass(c2)) =>
        toMod3(c1*c2)
    }
  }

  def inverse(m : Mod3) : Mod3 = m

  def division(p : Mod3, q : Mod3) : Mod3 = {
    (p,q) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (Mod3Top, _) => Mod3Top
      case (_, Mod3Top) => Mod3Top
      case (_, _) => Mod3Top 
    }
  }

  def remainder(m : Mod3, n : Mod3) : Mod3 = {
    (m,n) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (_, RestClass(0)) => Mod3Bottom
      case (RestClass(0), _) => RestClass(0)
      case (_, Mod3Top) => Mod3Top
      case (Mod3Top, _) => Mod3Top
      case (RestClass(p), RestClass(q)) => toMod3(p%q)
    }
  }

  def compare(p : Mod3, q : Mod3) : Option[Int] = {
    (p,q) match {
      case (Mod3Top, Mod3Top) => Option(0)
      case (Mod3Top, _) => Option(1)
      case (_, Mod3Top) => Option(-1)
      case (Mod3Bottom, Mod3Bottom) => Option(0)
      case (Mod3Bottom, _) => Option(-1)
      case (_, Mod3Bottom) => Option(1)
      case (_, _) => if(m == n) Option(0) else Option.empty
    }
  }

  override def top: Mod3 = Mod3Top

  override def bottom: Mod3 = Mod3Bottom
}