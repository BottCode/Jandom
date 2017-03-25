package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.DomainTransformation
import it.unich.jandom.domains.numerical.ProductDomain
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unipd.jandom.domains.numerical.congruence.CongruenceDomain

/**
  * Instantiation of the reduced product domain using the Congruence domain and the BoxDouble domain.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  * @param dom1 the first abstract domain
  * @param dom2 the second abstract domain
  */
class ProductCongruenceBoxDoubleDomain(override val dom1 : CongruenceDomain, override val dom2 : BoxDoubleDomain) extends ProductDomain[CongruenceDomain, BoxDoubleDomain](dom1, dom2) {
  override val dom1Todom2 = DomainTransformation.CongruenceToBoxDouble
  override val dom2Todom1 = DomainTransformation.BoxDoubleToCongruence

  /**
    * @inheritdoc
    */
  class ProductCongruenceBoxDouble(override val p1 : dom1.Property, override val p2 : dom2.Property) extends Product(p1, p2) {}

  /**
    * Class constructor
    */
  def apply(p1 : dom1.Property, p2 : dom2.Property) = new ProductCongruenceBoxDouble(p1, p2)

}  // end ProductCongruenceBoxDoubleDomain class

object ProductCongruenceBoxDoubleDomain {
  /**
    * Factory method
    */
  def apply() : ProductCongruenceBoxDoubleDomain = new ProductCongruenceBoxDoubleDomain(CongruenceDomain(), BoxDoubleDomain())
} // end ProductCongruenceBoxDoubleDomain companion object
