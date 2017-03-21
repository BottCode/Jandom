package it.unipd.jandom.domains.numerical.constant

/**
  * The elements of the constant domain.
  * It is a flat domain composed of constant elements, top and bottom.
  * This domain is useful for the constant propagation analysis.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object Constant {
  trait Constant
  // constant value
  case class Const (num : Int) extends Constant
  // no accurate info available for variable
  case object ConstantTop extends Constant
  // no possible value
  case object ConstantBottom extends Constant
}