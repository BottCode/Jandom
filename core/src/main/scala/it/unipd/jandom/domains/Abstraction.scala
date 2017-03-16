package it.unipd.jandom.domains

/**
  * Interface for domains which provide an abstraction operation.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
trait Abstraction[ConcreteType, AbstractType] {

  /**
    * Factory method for domain values (i.e. abstraction).
    *
    * @param num number that has to be converted to domain value
    * @return best approximation of the domain
    */
  def alpha(num : ConcreteType) : AbstractType
}
