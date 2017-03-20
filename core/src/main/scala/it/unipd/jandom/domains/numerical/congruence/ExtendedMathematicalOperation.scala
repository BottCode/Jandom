package it.unipd.jandom.domains.numerical.congruence

/**
  * Created by mirko on 3/18/17.
  * Implementation of the extended mathematical operations defined in Mine02 using the Option[Int]
  * to simulate the numbers
  */

class ExtendedMathematicalOperation {

  def isDivisor(y: Option[Int], y1: Option[Int]): Boolean =
    (y, y1) match {
      case (_, None) => true
      case (None, _) => false
      case (Some(v), Some(w)) => w % v == 0
    }

  def isCongruent(x : Int, x1: Int, y : Option[Int]) : Boolean = {
    if (x == x1)
      true
    else
      isDivisor(y, Some((x-x1).abs))
  }

  private def gcd(a : Int,b : Int) : Int = {
    if(b == 0)
      a
    else
      gcd(b, a % b)
  }

  private def lcm(a : Int, b : Int) : Int = {
    (a * b).abs / gcd(a,b)
  }

  def lcm(y : Option[Int], y1: Option[Int]): Option[Int] = {
    (y, y1) match {
      case (None, a) => None
      case (a, None) => None
      case (Some(a), Some(b)) => Some(lcm(a,b))
    }
  }
  def gcd(y : Option[Int], y1 : Option[Int]) : Option[Int] = {
    (y, y1) match {
      case (None, a) => a
      case (a, None) => a
      case (Some(a), Some(b)) => Some(gcd(a,b))
    }
  }

  def gcd(y : Option[Int], y1 : Option[Int], y2 : Option[Int]) : Option[Int] = {
    return gcd(y, gcd(y1, y2))
  }

  def *(y : Option[Int], z : Option[Int]): Option[Int] = {
    (y, z) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(a * b)
    }
  }

  /**
    * Implementation of the extended gcd. Conversion of the pseudo-code exposed in
    * https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
    * @param a
    * @param b
    * @return
    */
  def extendedGcd(a : Int, b : Int) : (Int, Int, Int) = {
      var s : Int = 0; var old_s : Int = 1;
      var t : Int = 0; var old_t : Int = 0;
      var r : Int = b; var old_r : Int = a;
      while(r != 0){
        var quotient : Int = old_r / r;
        (old_r, r) = (r, old_r - quotient * r)
        (old_s, s) = (s, old_s - quotient * s)
        (old_t, t) = (t, old_t - quotient * t)

      }
      (old_r, old_s, old_t) //greatest common divisor, bezout's coefficient 1, bezozt's coefficient 2
  }



}
object ExtendedMathematicalOperation{
  def apply() = new ExtendedMathematicalOperation()
}