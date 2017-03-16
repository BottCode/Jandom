package it.unipd.jandom.domains

/**
  * Interface for operators with basic algebraic operations.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
trait NumOperator [T] extends UniPDAnnotation {
  /**
    * Performs the (-) prefix operation.
    *
    * @param x variable that will be inverted
    * @return the inverse of `x`
    */
  def inverse(x : T) : T

  /**
    * Returns the sum of two domain variables.
    *
    * @param x the first term of the addition
    * @param y the second term of the addition
    * @return the result of the addition
    */
  def sum(x : T, y : T) : T

  /**
    * Returns the multiplication of two domain variables.
    *
    * @param x the first factor of the multiplication
    * @param y the second factor of the multiplication
    * @return the result of the multiplication
    */
  def mult(x : T, y : T) : T

  /**
    * Returns the division of two domain variables.
    *
    * @param x the numerator of the division
    * @param y the denominator of the division
    * @return the result of the division
    */
  def division(x : T, y : T) : T
}
