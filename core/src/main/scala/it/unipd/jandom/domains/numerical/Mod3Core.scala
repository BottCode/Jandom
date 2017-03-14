package it.unipd.jandom.domains.numerical

trait Mod3
case class RestClass(num : Int) extends Mod3
case object Mod3Bottom extends Mod3
case object Mod3Top extends Mod3

object Mod3DomainCore extends CompleteLatticeOperator[Mod3] with IntOperator[Mod3] {

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
      case (Mod3Bottom, a) => a
      case (a, Mod3Bottom) => a
      case (a,b) => if(a == b) a else Mod3Top
    }
  }

  def glb(m : Mod3, n : Mod3) : Mod3 = {
    (m, n) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (Mod3Top, a) => a
      case (a, Mod3Top) => a
      case (a, b) => if(a == b) a else Mod3Bottom
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
      case (Mod3Top, _) => Mod3Bottom
      case (_, Mod3Top) => Mod3Bottom
      case _ => Mod3Top //Because you cannot know if the result is an integer number. For instance 1/4 is neither even nor odd
    }
  }

  def remainder(p : Mod3, q : Mod3) : Mod3 = {
    (p,q) match {
      case (Mod3Bottom, _) => Mod3Bottom
      case (_, Mod3Bottom) => Mod3Bottom
      case (Odd, Odd) => Even
      case (_, _) => Mod3Top
      /*
        (Even, Odd) => Top
        (Mod3Top, Odd) => Top
        (_, Mod3Top) => Top
        (_, Even) => Top
      */
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
      case (a,b) => if(a.equals(b)) Option(0) else Option.empty
    }
  }

}