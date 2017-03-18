package it.unipd.jandom.domains.numerical.mod

object Mod3{
	trait Mod3
	case class RestClass(num : Int) extends Mod3
	case object Mod3Bottom extends Mod3
	case object Mod3Top extends Mod3
}