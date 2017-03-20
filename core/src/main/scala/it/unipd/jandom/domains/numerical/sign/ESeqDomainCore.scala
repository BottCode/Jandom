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
object ESeqDomainCore extends SignDomainCore {

  /**
    * @inheritdoc
    */
  override def sum(s: Sign, t: Sign) : Sign = {
    val result=super.sum(s,t)
    if(result == SignTop)
      (s,t) match {
        case (SignTop, _) => return SignTop
        case (_, SignTop) => return SignTop
        case (Geq0, Geq0) => return Geq0
        case (Geq0, Plus) => return Plus
        case (Plus, Geq0) => return Plus
        case (Leq0, Leq0) => return Leq0
        case (Leq0, Minus) => return Minus
        case (Minus, Leq0) => return Minus
        case _ => return SignTop
      }
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
        case (Plus, _) => return t
        case (_, Plus) => return s
        case (Minus, _) => return inverse(t)
        case (_, Minus) => return inverse(s)
        case (Leq0, Geq0) => return Leq0
        case (Geq0, Leq0) => return Leq0
        case _ => return SignTop
      }
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
        case (SignTop, _) => return SignTop
        case (_, SignTop) => return SignTop
        case (Plus, Minus) => return Leq0
        case (Minus, Plus) => return Leq0
        case (Plus, Geq0) => return Geq0
        case (Geq0, Plus) => return Geq0
        case (Minus, Geq0) => return Leq0
        case (Geq0, Minus) => return Leq0
        case (Plus, Leq0) => return Leq0
        case (Leq0, Plus) => return Leq0
        case (Minus, Leq0) => return Geq0
        case (Leq0, Minus) => return Geq0
        case (Leq0, Geq0) => return Leq0
        case (Geq0, Leq0) => return Leq0
        case (_, _) => if (s.equals(t)) return Geq0 else return SignTop
      }
    result
  }

  /**
    * @inheritdoc
    */
  override def remainder(s : Sign, t : Sign) : Sign = {
    val result=super.remainder(s,t)
    if(result == SignTop)
      (s, t) match {
        case (Plus, _) => return Geq0
        case (Geq0, _) => return Geq0
        case (Minus, _) => return Leq0
        case (Leq0, _) => return Leq0
        case (_, _) => return SignTop
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
        case (SignTop, _) => return SignTop
        case (_, SignTop) => return SignTop
        case (Zero, Plus) => return Geq0
        case (Plus, Zero) => return Geq0
        case (Zero, Minus) => return Leq0
        case (Minus, Zero) => return Leq0
        case (Plus, Minus) => return Neq0
        case (Minus, Plus) => return Neq0
        case (_, _) => compare(s, t) match {
          case Some(1) => return s
          case Some(0) => return s
          case Some(-1) => return t
          case _ => top
        }
      }
    result
  }

  /**
    * @inheritdoc
    */

  override def glb(s : Sign, t : Sign) : Sign = {
    val result = super.glb(s,t)
    if(result == SignTop)
      (s, t) match {
        case (SignTop, _) => return t
        case (_, SignTop) => return s
        case (Geq0, Leq0) => return Zero
        case (Leq0, Geq0) => return Zero
        case (_, _) => compare(s, t) match {
          case Some(-1) => return s
          case Some(0) => return s
          case Some(1) => return t
          case _ => return bottom
        }
      }
    else
      result;
    }

  override def compare(s: Sign, t:Sign): Option[Int] = {
    val result=super.compare(s,t)
    if(result.isEmpty)
      (s, t) match {
        case (Zero, Geq0) => return Option(-1)
        case (Plus, Geq0) => return Option(-1)
        case (Zero, Leq0) => return Option(-1)
        case (Minus, Leq0) => return Option(-1)
        case (Plus, Neq0) => return Option(-1)
        case (Minus, Neq0) => return Option(-1)
        case (Geq0, Zero) => return Option(1)
        case (Geq0, Plus) => return Option(1)
        case (Leq0, Zero) => return Option(1)
        case (Leq0, Minus) => return Option(1)
        case (Neq0, Plus) => return Option(1)
        case (Neq0, Minus) => return Option(1)
        case (_, _) => if (s.equals(t)) return Option(0) else return Option.empty
      }
    else
      result
  }

} // end object ESeqDomainCore
