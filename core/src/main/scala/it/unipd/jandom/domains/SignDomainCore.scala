package sign.main


object SignFunctions {

  trait Sign
  case object Plus extends Sign
  case object Minus extends Sign
  case object Zero extends Sign
  case object SignTop extends Sign
  case object SignBottom extends Sign

  def toSign(n : Int) : Sign = {
    if(n < 0)
      Minus
    else if(n == 0)
      Zero
    else
      Plus
  }

  def toSign(num : Double): Sign = {
    if(num > 0)
      Plus
    else if(num < 0)
      Minus
    else //Ignoring precision for now!
      Zero
  }


  def sum(s: Sign, t: Sign) : Sign = {
    println("sum called ")
    (s,t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (SignTop, _) => SignTop
      case (_, SignTop) => SignTop
      case (a, Zero) => a
      case (Zero, a) => a
      case (Plus, Plus) => Plus
      case (Minus, Minus) => Minus
      case _ => SignTop
    }
  }

  def mult(s: Sign, t : Sign) : Sign = {
    (s,t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (_, Zero) => Zero
      case (Zero, _) => Zero
      case (SignTop, _) => SignTop
      case (_, SignTop) => SignTop
      case (a, b) => if(a == b) Plus else Minus
    }
  }

  def inverse(s: Sign) : Sign = {
    s match {
      case Plus => Minus
      case Minus => Plus
      case a => a
    }
  }

  def division(s : Sign, t : Sign) : Sign = {
    (s, t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (_, Zero) => SignBottom
      case (SignTop, _) => SignTop
      case (Zero, SignTop) => Zero
      case (_, SignTop) => SignTop
      case (a, b) => if (a == b) Plus else Minus
    }
  }

  /** JAVA CONVENTION for mod/remainder sign is to take as result
    * the sign  of the dividend
    *
    * @param s
    * @param t
    * @return
    */
  def remainder(s : Sign, t : Sign) : Sign = {
    (s,t) match {
      case (SignBottom, _) => SignBottom
      case (_, SignBottom) => SignBottom
      case (a, _) => a
    }
  }

  def lub(s : Sign, t : Sign) = {
    (s, t) match {
      case (SignTop, _) => SignTop
      case (_, SignTop) => SignTop
      case (SignBottom, a) => a
      case (a, SignBottom) => a
      case (a, b) => if (a == b) a else SignTop
    }
  }

    def glb(s : Sign, t : Sign) = {
      (s,t) match {
        case (SignTop, a) => a
        case (a, SignTop) => a
        case (_, SignBottom) => SignBottom
        case (SignBottom, _) => SignBottom
        case (a, b) => if(a == b) a else SignBottom
      }
    }



  /*** XOR DISCLAIMER: If we do not distinguish between >= 0, and > 0 then
    * we take top because
    * 10 ^ 10 = 0
    * 10 ^ 11 = 1
    * therefore with + + we obtain top
    */

} // end object SignFunctions
