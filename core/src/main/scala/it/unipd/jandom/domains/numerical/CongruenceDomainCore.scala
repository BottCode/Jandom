package it.unipd.jandom.domains.numerical

/**
  * Created by mirko on 3/9/17.
  */
class CongruenceDomainCore {

  object CongruenceDomainCore {

    trait Congruence
    case class Mod(a : Int, b : Int) extends Congruence
    case object CongruenceBottom extends Congruence //Corresponds to the empty set


    def toCongruence(n : Int) : Congruence = {
      Mod(0, n)
    }


    /**
      * Greatest common divisor implementation taken from Rosetta https://rosettacode.org/wiki/Greatest_common_divisor
      */
    private def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)

    private def gcd(a : Int, b: Int, c : Int) : Int = gcd(a, gcd(b,c))

    def lub(c : Congruence, d : Congruence) : Congruence = {
      (c,d) match {
        case (CongruenceBottom, _) => c
        case (_, CongruenceBottom) => d
        case (Mod(a0, b0), Mod(a1,b1)) =>
          val a = gcd(a0, a1, (b0 - b1).abs)
          val b = Math.min(b0, b1)
          Mod(a, b)
      }
    }

    /**
      * Least common divisor implementation taken from Rosetta https://rosettacode.org/wiki/Least_common_multiple
      */
    private def lcm(a: Int, b: Int) : Int =(a*b).abs/gcd(a,b)


    def glb(c : Congruence, d : Congruence) :Congruence = {
      (c,d) match {
        case (CongruenceBottom, _) => CongruenceBottom
        case (_, CongruenceBottom) => CongruenceBottom
        case (Mod(a0, b0), Mod(a1, b1)) =>
          if(b0 == b1 % gcd(a0,a1)) {
            val a = lcm(a0, a1)
            val b = b1 % a1
            Mod(a,b)
          }
          else {
            CongruenceBottom
          }
      }
    }

    def sum(c : Congruence, d : Congruence) : Congruence = {
      (c,d) match {
        case (CongruenceBottom, _) => CongruenceBottom
        case (_, CongruenceBottom) => CongruenceBottom
        case (Mod(a0, b0), Mod(a1, b1)) =>
          val a = gcd(a0,a1)
          val b = Math.min(b0,b1)
          Mod(a, b)
      }
    }

    def inverse(c : Congruence) : Congruence = {
      c match {
        case CongruenceBottom => CongruenceBottom
        case Mod(a, b) =>
          Mod(a, -b)
      }
    }


    def mult(c : Congruence, d : Congruence) : Congruence = {
      (c,d) match {
        case (CongruenceBottom, _) => CongruenceBottom
        case (_, CongruenceBottom) => CongruenceBottom
        case (Mod(a0, b0), Mod(a1, b1)) =>
          val a = gcd(a0*a1, a0*b1, a1*b0)
          val b = b0*b1
          Mod(a, b)
      }
    }

    def division(c : Congruence, d : Congruence) : Congruence = {
      (c,d) match {
        case (CongruenceBottom, _) => CongruenceBottom
        case (_, CongruenceBottom) => CongruenceBottom
        case (_, _) => Mod(1, 0) //top element
      }
    }

    def remainder(c : Congruence, d : Congruence) : Congruence = {
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
        case Mod(a, b) => a + "Z + " + b
      }

    }

    /* Maybe we want to standardize all the Mods, before processing them */
    def standardForm(c : Congruence) : Congruence = {
      c match {
        case CongruenceBottom => CongruenceBottom
        case Mod(a,b) =>
          if(a != 0)
            Mod(a, b % a)
          else
            Mod(a,b)
      }
    }

    def compare(c : Congruence, d : Congruence) : Option[Int] = {
      (standardForm(c), standardForm(d)) match {
        case (CongruenceBottom, CongruenceBottom) => Option(0)
        case (CongruenceBottom, _) => Option(-1)
        case (_, CongruenceBottom) => Option(1)
        case (Mod(a0, b0), Mod(a1, b1)) =>

          if(a0 == a1 && b0 == b1)
            Option(0)
          else if(a0 == a1 && b0 != b1)
            Option.empty
          else if(a0 != a1 && b0 == b1)
            Option.empty //TODO Don't know what to do
          else
            Option.empty //TODO Don't know what to do


      }
    }

  } // end object SignFunctions


}
