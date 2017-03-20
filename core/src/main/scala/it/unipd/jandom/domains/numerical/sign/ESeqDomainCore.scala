package it.unipd.jandom.domains.numerical.sign

import it.unipd.jandom.domains.numerical.sign.Sign._
import ESeq._

/**
  * Operations on the extended domain of signs with >=0, <=0 and !=0.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ESeqDomainCore extends SignDomainCore {

  /**
    * @inheritdoc
    */
  override def sum(s: Sign, t: Sign) : Sign = {
    val result=super.sum(s,t)
    if(result == SignTop)
      (s,t) match {
        case (SignTop, _) => SignTop
        case (_, SignTop) => SignTop
        case (Geq0, Geq0) => Geq0
        case (Geq0, Plus) => Plus
        case (Plus, Geq0) => Plus
        case (Leq0, Leq0) => Leq0
        case (Leq0, Minus) => Minus
        case (Minus, Leq0) => Minus
        case _ => SignTop
      }
    else
      result
  }
  
  /**
    * @inheritdoc
    */
  override def mult(s: Sign, t : Sign) : Sign = {
    val result=super.mult(s,t)
    //  if result == Top => s==Top || t==Top
    if(result == Plus || result == Minus)
      (s,t) match {
        case (Plus, _) => t
        case (_, Plus) => s
        case (Minus, _) => inverse(t)
        case (_, Minus) => inverse(s)
        case (Leq0, Geq0) => Leq0
        case (Geq0, Leq0) => Leq0
        case _ => SignTop
      }
    else
      result
  }

  /**
    * @inheritdoc
    */
  override def inverse(s: Sign) : Sign =
    super.inverse(s) match {
        case Leq0 => Geq0
        case Geq0 => Leq0
        case a => a
      }

  /**
    * @inheritdoc
    */
  override def division(s : Sign, t : Sign) : Sign = {
    val result=super.division(s,t)
    if(result == SignTop)
      (s, t) match {
        case (SignTop, _) => SignTop
        case (_, SignTop) => SignTop
        case (Plus, Minus) => Leq0
        case (Minus, Plus) => Leq0
        case (Plus, Geq0) => Geq0
        case (Geq0, Plus) => Geq0
        case (Minus, Geq0) => Leq0
        case (Geq0, Minus) => Leq0
        case (Plus, Leq0) => Leq0
        case (Leq0, Plus) => Leq0
        case (Minus, Leq0) => Geq0
        case (Leq0, Minus) => Geq0
        case (Leq0, Geq0) => Leq0
        case (Geq0, Leq0) => Leq0
        case (_, _) => if (s.equals(t)) Geq0 else SignTop
      }
    else
      result
  }

  /**
    * @inheritdoc
    */
  override def remainder(s : Sign, t : Sign) : Sign = {
    val result=super.remainder(s,t)
    if(result == SignTop)
      (s, t) match {
        case (Plus, _) => Geq0
        case (Geq0, _) => Geq0
        case (Minus, _) => Leq0
        case (Leq0, _) => Leq0
        case (_, _) => SignTop
      }
    else
      result
  }

  /**
    * @inheritdoc
    */
  override def lub(s : Sign, t : Sign) : Sign = {
    println(s"Entering Lub")
    val result = super.lub(s,t)
    println(s"Lub between $s and $t: $result")
    if(result == SignTop)
      (s, t) match {
        case (SignTop, _) => SignTop
        case (_, SignTop) => SignTop
        case (Zero, Plus) => Geq0
        case (Plus, Zero) => Geq0
        case (Zero, Minus) => Leq0
        case (Minus, Zero) => Leq0
        case (Plus, Minus) => Neq0
        case (Minus, Plus) => Neq0
        case (_, _) => compare(s, t) match {
          case Some(1) => s
          case Some(0) => s
          case Some(-1) => t
          case _ => top
        }
      }
    else
      result
  }

  /**
    * @inheritdoc
    */

  override def glb(s : Sign, t : Sign) : Sign = {
    val result = super.glb(s,t)
    if(result == SignTop)
      (s, t) match {
        case (SignTop, _) => t
        case (_, SignTop) => s
        case (Geq0, Leq0) => Zero
        case (Leq0, Geq0) => Zero
        case (_, _) => compare(s, t) match {
          case Some(-1) => s
          case Some(0) => s
          case Some(1) => t
          case _ => bottom
        }
      }
    else
      result;
    }

  override def compare(s: Sign, t:Sign): Option[Int] = {
    val result=super.compare(s,t)
    if(result.isEmpty)
      (s, t) match {
        case (Zero, Geq0) => Option(-1)
        case (Plus, Geq0) => Option(-1)
        case (Zero, Leq0) => Option(-1)
        case (Minus, Leq0) => Option(-1)
        case (Plus, Neq0) => Option(-1)
        case (Minus, Neq0) => Option(-1)
        case (Geq0, Zero) => Option(1)
        case (Geq0, Plus) => Option(1)
        case (Leq0, Zero) => Option(1)
        case (Leq0, Minus) => Option(1)
        case (Neq0, Plus) => Option(1)
        case (Neq0, Minus) => Option(1)
        case (_, _) => if (s.equals(t)) Option(0) else Option.empty
      }
    else
      result
  }

} // end object ESeqDomainCore
object ESeqDomainCore {
  def apply() = new ESeqDomainCore
}