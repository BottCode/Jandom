/*package it.unipd.jandom.domains.numerical.mod

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import Mod3._

/**
  * @author $assume
  */

class Mod3DomainCore extends CompleteLatticeOperator[Mod3] with IntOperator[Mod3] with Abstraction[Int, Mod3]{

  override def alpha(t : Int) : Mod3 =
    if(t % 3 < 0)
      RestClass(3 + (t % 3))
    else
      RestClass(t % 3)

  override def sum(m : Mod3, n : Mod3) : Mod3 = {
    (m, n) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (Mod3Top, _) => Mod3Top
      case (_, Mod3Top) => Mod3Top
      case (RestClass(m1), RestClass(n1)) =>
        alpha(m1+n1)
    }
  }

  override def lub(m : Mod3, n : Mod3) : Mod3 = {
    (m,n) match {
      case (Mod3Top, _) => Mod3Top
      case (_, Mod3Top) => Mod3Top
      case (Mod3Bottom, _) => n
      case (_, Mod3Bottom) => m
      case (_, _) => if(m == n) m else Mod3Top
    }
  }

  override def glb(m : Mod3, n : Mod3) : Mod3 = {
    (m, n) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (Mod3Top, _) => n
      case (_, Mod3Top) => m
      case (_, _) => if(m == n) m else Mod3Bottom
    }
  }

  override def mult(m : Mod3, n : Mod3) : Mod3 = {
    (m, n) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (RestClass(0), _) => RestClass(0)
      case (_, RestClass(0)) => RestClass(0)
      case (Mod3Top, _) => Mod3Top
      case (_, Mod3Top) => Mod3Top
      case (RestClass(m1), RestClass(n1)) =>
        alpha(m1*n1)
    }
  }

  override def inverse(m : Mod3) : Mod3 = {
    m match {
      case RestClass(1) => RestClass(2)
      case RestClass(2) => RestClass(1)
      case _ => m
    }
  }

  override def division(m : Mod3, n : Mod3) : Mod3 = {
    (m,n) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (Mod3Top, _) => Mod3Top
      case (_, Mod3Top) => Mod3Top
      case (_, _) => Mod3Top 
    }
  }

  override def remainder(m : Mod3, n : Mod3) : Mod3 = {
    (m,n) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (_, RestClass(0)) => Mod3Top
      case (RestClass(0), _) => Mod3Top
      case (_, Mod3Top) => Mod3Top
      case (Mod3Top, _) => Mod3Top
      case (RestClass(m1), RestClass(n1)) => alpha(m1%n1)
    }
  }

  override def compare(m : Mod3, n : Mod3) : Option[Int] = {
    (m,n) match {
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
} //end class Mod3DOmainCore

object Mod3DomainCore {
  def apply() = new Mod3DomainCore
}*/