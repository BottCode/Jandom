package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.DomainTransformation
import it.unich.jandom.domains.numerical.ProductDomain
import it.unipd.jandom.domains.numerical.mod.ModKDomain
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
class ProductSignModKDomain(override val dom1 : SignDomain, override val dom2 : ModKDomain) extends ProductDomain[SignDomain, ModKDomain](dom1, dom2) {
  override val dom1Todom2 = DomainTransformation.SignToModK
  override val dom2Todom1 = DomainTransformation.ModKToSign

  /**
    * @inheritdoc
    */
  class ProductSignModK(override val p1 : dom1.Property, override val p2 : dom2.Property) extends Product(p1, p2) {}

  /**
    * Class constructor
    */
  def apply(p1 : dom1.Property, p2 : dom2.Property) = new ProductSignModK(p1, p2)

}  // end ProductSignModK class

object ProductSignModKDomain {


  /**
    * Factory method
    */
  def apply(num : Int) : ProductSignModKDomain = new ProductSignModKDomain(SignDomain(), ModKDomain(num))
} // end ProductSignParityDomain companion object
