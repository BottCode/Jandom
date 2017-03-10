package it.unipd.jandom.domains

import it.unich.jandom.domains.numerical.{NumericalDomain, NumericalProperty}
import it.unipd.jandom.domains.ConstantDomainCore._
import it.unipd.jandom.domains.constantsDomain.Property
import it.unipd.jandom.domains.constantsDomainCore.constants

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
    
    override def addVariable(): Property = {
      if (unreachable)
        return ConstantDomain.this.bottom(dimension + 1)
      ConstantDomain.this(constants :+ ConstantTop)
    }
    
    override def delVariable(index: Int): Property = {
      require(index < constants.length && index >= 0)
      val result = new Array[Constant](constants.length - 1)
      // Copy the first index-1 elements
      Array.copy(constants, 0, result, 0, index)
      // Copy the remaining elements
      Array.copy(constants, index + 1, result, index, constants.length - index - 1)
      Property(result, unreachable)
    }
    
    def mapVariables(rho: Seq[Int]): Property =  {
      require(rho.length == dimension)
      val resultDim = rho.count(_ >= 0)
      require(rho forall { i => i >= -1 && i < resultDim })
      // we do not check injectivity
      val result = new Array[Constant](resultDim)
      for ((resultIndex, i) <- rho.zipWithIndex; if resultIndex >= 0)
        result(resultIndex) = constants(i)
      Property(result, unreachable)
    }
    
  }
}
