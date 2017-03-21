package it.unipd.jandom.domains

/**
  * Interface for operators with basic algebraic operations.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
trait NumOperator[AbstractType]{
  /**
    * Performs the (-) prefix operation.
    *
    * @param x variable that will be inverted
    * @return the inverse of `x`
    */
  def inverse(x : AbstractType) : AbstractType

  /**
    * Returns the sum of two domain variables.
    *
    * @param x the first addend
    * @param y the second addend
    * @return the result of the addition
    */
  def sum(x : AbstractType, y : AbstractType) : AbstractType

  /**
    * Returns the multiplication of two domain variables.
    *
    * @param x the first factor
    * @param y the second factor
    * @return the product
    */
  def mult(x : AbstractType, y : AbstractType) : AbstractType

  /**
    * Returns the division of two domain variables.
    *
    * @param x the dividend
    * @param y the divisor
    * @return the result of the division
    */
  def division(x : AbstractType, y : AbstractType) : AbstractType
}
