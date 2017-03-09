package it.unipd.jandom.domains

/**
  * Constant domain
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>, Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *           Stefano Munari <stefano.munari.1@studenti.unipd.it>
  */
object ConstantDomainCore {
  trait Constant
  case class Constant (num : Numeric) extends Constant
  case object ConstantTop extends Constant
  case object ConstantBottom extends Constant

  def toConstant(num : Int) : Constant = Constant(num)

  def toConstant(num : Double) : Constant = Constant(num)

}
