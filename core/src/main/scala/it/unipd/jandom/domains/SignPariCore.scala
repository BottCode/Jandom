package sign

trait Parity;
case object Odd extends Parity;
case object Even extends Parity;
case object Top extends Parity;
case object Bottom extends Parity;

/**
  * This class models the
  */
class Pari {
   def toParity(n : Int) : Parity = {
     if(n % 2 == 1)
       Odd
     else
       Even
   }
   
   
   
   def sum(s: Parity, t : Parity) : Parity = {
    (s,t) match {
      case (Bottom, _) => Bottom
      case (_, Bottom) => Bottom
      case (Top, _) => Top
      case (_, Top) => Top
      case (a, b) => if(a == b) Even else Odd
    }
  }
   
   def inverse(s: Parity) : Parity = s
   
   def mult (s : Parity, t : Parity) : Parity = {
     (s,t) match {
       case (Bottom, _) => Bottom
       case (_, Bottom) => Bottom
       case (Top, _) => Top
       case (_, Top) => Top
       case (a, b) => if(a == b) a else Even
     }
   }
   
   def div(s : Parity, t : Parity) : Parity = {
     (s, t) match {
       case (Bottom, _) => Bottom
       case (_, Bottom) => Bottom
       case (_, _) => Top
       
     }
   }


}
