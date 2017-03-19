package it.unipd.jandom.domains.numerical.mod

object ModK {
	private var k = 1

	trait ModK
	case class RestClass(num : Int) extends ModK
	case object ModKBottom extends ModK
	case object ModKTop extends ModK

	def apply(num : Int) {
		k = num
	}

	def divisor: Int = k
}