package it.unipd.jandom.domains

/**
  * Interface for operators with the modulo operation.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
trait IntOperator[T] extends NumOperator[T] {
  /**
    * Modulo operation between two values.
    * @param x value "on the left" of %
    * @param y modulus
    * @return the remainder of x % y
    */
  def remainder(x : T, y : T) : T
}
