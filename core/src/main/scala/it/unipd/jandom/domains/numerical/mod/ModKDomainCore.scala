package it.unipd.jandom.domains.numerical.mod

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import ModK._

/**
  * @author $assume
  */

object ModKDomainCore extends CompleteLatticeOperator[ModK] 
  with IntOperator[ModK] with Abstraction[Int, ModK]{

  override def alpha(t : Int) : ModK = {
    if(divisor == 0)
      return ModKTop      
    RestClass(((t % divisor) + divisor) % divisor)
  }
  
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

  override def lub(m : ModK, n : ModK) : ModK = {
    (m,n) match {
      case (ModKTop, _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (ModKBottom, _) => n
      case (_, ModKBottom) => m
      case (_, _) => if(m == n) m else ModKTop
    }
  }

  override def glb(m : ModK, n : ModK) : ModK = {
    (m, n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (ModKTop, _) => n
      case (_, ModKTop) => m
      case (_, _) => if(m == n) m else ModKBottom
    }
  }

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

  override def inverse(m : ModK) : ModK = {
    m match {
      case RestClass(n) => alpha((divisor - n) % divisor)
      case _ => m
    }
  }

  override def division(m : ModK, n : ModK) : ModK = {
    (m,n) match {
      case (ModKBottom, _) => ModKBottom
      case (_, ModKBottom) => ModKBottom
      case (ModKTop, _) => ModKTop
      case (_, ModKTop) => ModKTop
      case (_, _) => ModKTop
    }
  }

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

  override def top: ModK = ModKTop

  override def bottom: ModK = ModKBottom

  def apply(num : Int) = ModK(num)
}