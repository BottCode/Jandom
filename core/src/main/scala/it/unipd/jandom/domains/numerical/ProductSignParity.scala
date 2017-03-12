package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.numerical.{NumericalProperty, SumDomain}
import it.unipd.jandom.domains.numerical.ParityDomainCore.Parity
import it.unipd.jandom.domains.numerical.SignDomainCore.Sign



class SumSignParity extends SumDomain[SignDomain, ParityDomain]{
  override val dom1: SignDomain = SignDomain()
  override val dom2: ParityDomain = ParityDomain()
  case class Property private[SumSignParity](sign : Array[Sign], parity : Array[Parity], unreachable: Boolean) extends Sum {
    override val p1: dom1.Property = dom1.Property
    override val p2: dom2.Property = dom2.Property
  }


    override def apply(p1: dom1.Property, p2: dom2.Property): Property = ???



}
object SumSignParity {
  def apply() = new SumSignParity()
}