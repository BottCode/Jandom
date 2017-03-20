package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.DomainTransformation
import it.unich.jandom.domains.numerical.ProductDomain
import it.unipd.jandom.domains.numerical.parity.ParityDomain
import it.unipd.jandom.domains.numerical.sign.SignDomain

/**
  * Instantiation of the reduced product domain using the Sign domain and the Parity domain.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  * @param dom1 the first abstract domain
  * @param dom2 the second abstract domain
  */
class ProductSignParityDomain(override val dom1 : SignDomain, override val dom2 : ParityDomain) extends ProductDomain[SignDomain, ParityDomain](dom1, dom2) {

  override val dom1Todom2 = DomainTransformation.SignToParity
  override val dom2Todom1 = DomainTransformation.ParityToSign

  /**
    * @inheritdoc
    */
  class ProductSignParity(override val p1 : dom1.Property, override val p2 : dom2.Property) extends Product(p1, p2) {}

  /**
    * Class constructor
    */
  def apply(p1 : dom1.Property, p2 : dom2.Property) = new ProductSignParity(p1, p2)
} // end ProductSignParityDomain class

object ProductSignParityDomain {
  private lazy val domain = new ProductSignParityDomain(SignDomain(), ParityDomain())

  /**
    * Factory method
    */
  def apply() : ProductSignParityDomain = domain
} // end ProductSignParityDomain companion object
