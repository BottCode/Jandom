package it.unipd.jandom.domains

import it.unich.jandom.domains.numerical.{NumericalDomain, NumericalProperty}
import it.unipd.jandom.domains.ConstantDomainCore._

/**
  * Constant domain
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>, Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *           Stefano Munari <stefano.munari.1@studenti.unipd.it>
  */
object ConstantDomain extends NumericalDomain{

  def apply(constants : Array[Constant]): Property = Property(constants, constants.forall( _.equals(ConstantBottom)))

  def top(num: Int) = Property(Array.fill(num)(ConstantTop), unreachable = false)

  def bottom(num: Int) = Property(Array.fill(num)(ConstantBottom), unreachable = true)

  case class Property private[ConstantDomain] (constants: Array[Constant], unreachable: Boolean) extends NumericalProperty[Property] {

    override type Domain = ConstantDomain.type

    /**
      * @inheritdoc
      */
    override def isPolyhedral: Boolean = false

    /**
      * @inheritdoc
      */
    override def addVariable(): Property =
      if (unreachable)
        ConstantDomain.this.bottom(dimension + 1)
      else
        ConstantDomain.this(Constant :+ ConstantTop)
  }
}
