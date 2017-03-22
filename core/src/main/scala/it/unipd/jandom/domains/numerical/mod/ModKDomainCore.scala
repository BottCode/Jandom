package it.unipd.jandom.domains.numerical.mod

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import ModK._

/**
  * Domain with values with the form `a + K * Z`.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object ModKDomainCore extends CompleteLatticeOperator[ModK]
  with IntOperator[ModK] with Abstraction[Int, ModK]{

  /**
    * @inheritdoc
    */
  override def alpha(t : Int) : ModK = {
    if(divisor == 0)
      return ModKTop      
    RestClass(((t % divisor) + divisor) % divisor)
  }

  /**
    * @inheritdoc
    */
  override def sum(m : ModK, n : ModK) : ModK = {
    (m, n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (ModKTop, _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (RestClass(m1), RestClass(n1)) =>
        alpha(m1+n1)
    }
  }

  /**
    * @inheritdoc
    */
  override def inverse(m : ModK) : ModK = {
    m match {
      case RestClass(n) => alpha((divisor - n) % divisor)
      case _ => m
    }
  }

  /**
    * @inheritdoc
    */
  override def mult(m : ModK, n : ModK) : ModK = {
    (m, n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (RestClass(0), _) => RestClass(0)
      case (_, RestClass(0)) => RestClass(0)
      case (ModKTop, _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (RestClass(m1), RestClass(n1)) =>
        alpha(m1*n1)
    }
  }

  /**
    * @inheritdoc
    */
  override def division(m : ModK, n : ModK) : ModK = {
    (m,n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (ModKTop, _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (_, _) => ModKTop
    }
  }

  /**
    * @inheritdoc
    */
  override def remainder(m : ModK, n : ModK) : ModK = {
    (m,n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (_, RestClass(0)) => ModKTop
      case (RestClass(0), _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (ModKTop, _) => ModKTop
      case (RestClass(m1), RestClass(n1)) => alpha(m1%n1)
    }
  }

  /**
    * @inheritdoc
    */
  override def lub(m : ModK, n : ModK) : ModK = {
    (m,n) match {
      case (ModKTop, _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (ModKBottom, _) => n
      case (_, ModKBottom) => m
      case (_, _) => if(m == n) m else ModKTop
    }
  }

  /**
    * @inheritdoc
    */
  override def glb(m : ModK, n : ModK) : ModK = {
    (m, n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (ModKTop, _) => n
      case (_, ModKTop) => m
      case (_, _) => if(m == n) m else ModKBottom
    }
  }

  /**
    * @inheritdoc
    */
  override def compare(m : ModK, n : ModK) : Option[Int] = {
    (m,n) match {
      case (ModKTop, ModKTop) => Option(0)
      case (ModKTop, _) => Option(1)
      case (_, ModKTop) => Option(-1)
      case (ModKBottom, ModKBottom) => Option(0)
      case (ModKBottom, _) => Option(-1)
      case (_, ModKBottom) => Option(1)
      case (_, _) => if(m == n) Option(0) else Option.empty
    }
  }

  /**
    * @inheritdoc
    */
  override def top: ModK = ModKTop

  /**
    * @inheritdoc
    */
  override def bottom: ModK = ModKBottom

  def apply(num : Int) = ModK(num)
}

