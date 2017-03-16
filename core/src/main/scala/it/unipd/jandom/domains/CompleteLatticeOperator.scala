package it.unipd.jandom.domains

/**
  * Interface for domains which are complete lattices.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
trait CompleteLatticeOperator [T] {

  /**
    * Returns the least upper bound between two lattice values.
    *
    * @param x first term of the lub
    * @param y second term of the lub
    * @return the lub of `x` and `y`
    */
  def lub(x : T, y : T) : T

  /**
    * Returns the greatest lower bound between two lattice values.
    *
    * @param x first term of the glb
    * @param y second term of the glb
    * @return the glb of `x` and `y`
    */
  def glb(x : T, y : T) : T

  /**
    * Performs a Scala-like comparison (same behaviour as Java's compareTo) between two lattice values.
    *
    * @param x left hand side
    * @param y right hand side
    * @return 1 if `x` > `y` -- 0 if `x` = `y` -- -1 if `x` < `y`
    */
  def compare(x : T, y : T) : Option[Int]

  /**
    * Maximum element of this lattice.
    * @return the top element
    */
  def top : T

  /**
    * Least element of this lattice.
    * @return the bottom element
    */
  def bottom : T
}
