package it.unipd.jandom.domains.numerical.congruence

/**
  * Created by mirko on 3/18/17.
  * Implementation of the extended mathematical operations defined in Mine02 using the Option[Int]
  * to simulate the N* U infinite
  */

class ExtendedMathematicalOperation {

  /**
    * Implementation of the extended gcd. Conversion of the pseudo-code exposed in
    * https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
    * @param _a
    * @param _b
    * @return
    */
  def extendedGcd(_a : Option[Int], _b : Option[Int]) : (Int, Option[Int], Option[Int]) = {
    val a = _a match {
      case None => 0
      case Some(x) => x
    }
    val b = _b match {
      case None => 0
      case Some(x) => x
    }
    // REAL ALGORITHM
    var s : Int = 0
    var old_s : Int = 1
    var t : Int = 0
    var old_t : Int = 0
    var r : Int = b
    var old_r : Int = a
    while(r != 0){
      val quotient : Int = old_r / r;
      val (tmp_old_r, tmp_r) = (r, old_r - quotient * r)
      val (tmp_old_s, tmp_s) = (s, old_s - quotient * s)
      val (tmp_old_t, tmp_t) = (t, old_t - quotient * t)

      old_r = tmp_old_r
      r = tmp_r
      old_s = tmp_old_s
      s = tmp_s
      old_t = tmp_old_t
      t = tmp_t
    }

    val res_s = old_s match {
      case 0 => None
      case _ => Some(old_s)
    }
    val res_t = old_t match {
      case 0 => None
      case _ => Some(old_t)
    }
    (old_r, res_s, res_t) //greatest common divisor, bezout's coefficient 1, bezozt's coefficient 2
  }


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

  def +(y : Option[Int], z : Option[Int]) : Option[Int] = {
    (y, z) match {
      case (None, _) => z
      case (_, None) => y
      case (Some(a), Some(b)) => Some(a + b)
    }
  }

  def division(y : Option[Int], z : Option[Int]) : Option[Int] = {
    (y, z) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(a/b)
    }
  }




}
object ExtendedMathematicalOperation{
  def apply() = new ExtendedMathematicalOperation()
}