package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.DomainTransformation
import it.unich.jandom.domains.numerical.ProductDomain

class ProductSignParityDomain(override val dom1 : SignDomain, override val dom2 : ParityDomain) extends ProductDomain[SignDomain, ParityDomain](dom1, dom2) {

  override val dom1Todom2 = DomainTransformation.SignToParity
  override val dom2Todom1 = DomainTransformation.ParityToSign

  class ProductSignParity(override val p1 : dom1.Property, override val p2 : dom2.Property) extends Product(p1, p2) {}

  def apply(p1 : dom1.Property, p2 : dom2.Property) = new ProductSignParity(p1, p2)
}

object ProductSignParityDomain {
  private lazy val v = new ProductSignParityDomain(SignDomain(), ParityDomain())

  def apply() = v
}
