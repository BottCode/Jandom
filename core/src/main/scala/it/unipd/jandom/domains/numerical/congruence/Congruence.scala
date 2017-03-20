package it.unipd.jandom.domains.numerical.congruence

trait Congruence
case class Mod(a : Option[Int], b : Int) extends Congruence {
  require(a match { // Checking if the value is bigger than 0 as in Mine02
    case None => true
    case Some(x) => x > 0
  })
}
case object CongruenceBottom extends Congruence //Corresponds to the empty set
