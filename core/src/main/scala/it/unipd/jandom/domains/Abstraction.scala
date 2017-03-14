package it.unipd.jandom.domains

/**
  * Created by m0bius on 3/14/17.
  */
trait Abstraction[ConcreteType, AbstractType] {
  def alpha(num : ConcreteType) : AbstractType
}
