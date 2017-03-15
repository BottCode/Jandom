package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.numerical._


class SumSignParityDomain(val dom1: SignDomain, val dom2: ParityDomain) extends SumDomain[SignDomain, ParityDomain]{

  type Property = SumSignParity

  class SumSignParity(val p1: dom1.Property, val p2: dom2.Property) extends Sum {

  }

  def apply(p1: dom1.Property, p2: dom2.Property) = new SumSignParity(p1, p2)
}

object SumSignParityDomain {
  private lazy val v = new SumSignParityDomain(SignDomain(), ParityDomain())

  def apply() : SumSignParityDomain = v
}
