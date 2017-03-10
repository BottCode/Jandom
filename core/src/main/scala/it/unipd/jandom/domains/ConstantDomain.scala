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

  case class Property private[ConstantDomain]
    (constants: Array[Constant], unreachable: Boolean) extends NumericalProperty[Property] {

    override type Domain = ConstantDomain.type

    override def isPolyhedral: Boolean = false

    /**
      * @inheritdoc
      */
    def dimension: Int = constants.length

    /**
      * @inheritdoc
      */
    def domain = ConstantDomain

    def isEmpty: Boolean = unreachable

    def isTop: Boolean = !isEmpty && constants.forall( _.equals(ConstantTop))

    def isBottom: Boolean = isEmpty

    def bottom: Property = ConstantDomain.bottom(constants.length)

    def top: Property = ConstantDomain.top(constants.length)

    /**
      * @inheritdoc
      */
    override def addVariable(): Property = {
      if (unreachable)
        return ConstantDomain.this.bottom(dimension + 1)
      ConstantDomain.this(constants :+ ConstantTop)
    }
  }
}
