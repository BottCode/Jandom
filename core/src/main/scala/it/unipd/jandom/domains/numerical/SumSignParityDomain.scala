package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.numerical.SumDomain
import it.unipd.jandom.domains.numerical.parity.ParityDomain
import it.unipd.jandom.domains.numerical.sign.SignDomain

/**
  * Instantiation of the sum domain using the Sign domain and the Parity domain.
  *
  * @param dom1 the first abstract domain
  * @param dom2 the second abstract domain
  */
class SumSignParityDomain(val dom1: SignDomain, val dom2: ParityDomain) extends SumDomain[SignDomain, ParityDomain]{

  type Property = SumSignParity

  /**
    * @inheritdoc
    */
  class SumSignParity(val p1: dom1.Property, val p2: dom2.Property) extends Sum { }

  def apply(p1: dom1.Property, p2: dom2.Property) = new SumSignParity(p1, p2)
} // end SumSignParityDomain class

object SumSignParityDomain {
  private lazy val v = new SumSignParityDomain(SignDomain(), ParityDomain())

  def apply() : SumSignParityDomain = v
} // end SumSignParityDomain companion object
