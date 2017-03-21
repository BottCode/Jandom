package it.unipd.jandom.domains.numerical.congruence

trait Congruence
case class Mod(a : Option[Int], b : Int) extends Congruence {

  require(a match {
    // Checking if the value is bigger than 0 as in Mine02
    case None => true
    case Some(x) => x > 0
  })

  override def toString: String = {

    (a, b) match {
      case (None, 0) => 0 + ""
      case (None, x) => x + ""
      case (Some(1), 0) => "Z"
      case (Some(1), x) => "Z + " + x
      case (Some(a), x) => a + "Z + " + x
    }
  }
}
case object CongruenceBottom extends Congruence {  //Corresponds to the empty set
  override def toString: String = "BOTTOM"
}


