package it.unipd.jandom.domains.numerical.constant

import it.unipd.jandom.domains.{Abstraction, CompleteLatticeOperator, IntOperator}
import Constant._

/**
  * Constant domain, checks if a given variable is constant 
  * (and equal to a value `num`) in a program point.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
class ConstantDomainCore extends CompleteLatticeOperator[Constant]
  with IntOperator[Constant] with Abstraction[Int, Constant]{

  /**
    * @inheritdoc
    */
  def alpha(num : Int) : Constant = Const(num)

  /**
    * @inheritdoc
    */
  def sum(x : Constant, y : Constant) : Constant = {
    (x,y) match {
      case (ConstantBottom, _) => ConstantBottom
      case (_, ConstantBottom) => ConstantBottom
      case (ConstantTop, _) => ConstantTop
      case (_, ConstantTop) => ConstantTop
      case (Const(a), Const(b)) => Const(a+b)
    }
  }

  /**
    * @inheritdoc
    */
  def inverse(x: Constant) : Constant = {
    x match {
      case ConstantTop => ConstantTop
      case ConstantBottom => ConstantBottom
      case Const(a) => Const(-a)
    }
  }

  /**
    * @inheritdoc
    */
  def mult(x : Constant, y : Constant) : Constant = {
    (x, y) match {
      case (ConstantBottom, _) => ConstantBottom
      case (_, ConstantBottom) => ConstantBottom
      case (Const(0), _) => x
      case (_, Const(0)) => y
      case (ConstantTop, _) => ConstantTop
      case (_, ConstantTop) => ConstantTop
      case (Const(a), Const(b)) => Const(a*b)
    }
  }

  /**
    * @inheritdoc
    */
  def division(x : Constant, y : Constant) : Constant = {
    (x, y) match {
      case (ConstantBottom, _) => ConstantBottom
      case (_, ConstantBottom) => ConstantBottom
      case (_, Const(0)) => ConstantBottom
      case (Const(0), _) => Const(0)
      case (ConstantTop, _) => ConstantTop
      case (_, ConstantTop) => ConstantTop
      case (Const(a), Const(b)) => Const(a/b)
    }
  }

  /**
    * @inheritdoc
    */
  def remainder(x : Constant, y : Constant) : Constant = {
    (x,y) match {
      case (ConstantBottom, _) => ConstantBottom
      case (_, ConstantBottom) => ConstantBottom
      case (_, Const(0)) => ConstantBottom
      case (Const(0), _) => Const(0)
      case (_, ConstantTop) => ConstantTop
      case (ConstantTop, _) => ConstantTop
      case (Const(a), Const(b)) => Const(a%b)
    }
  }

  /**
    * @inheritdoc
    */
  def lub(x : Constant, y : Constant) : Constant = {
    (x, y) match {
      case (ConstantTop, _) => ConstantTop
      case (_, ConstantTop) => ConstantTop
      case (ConstantBottom, _) => y
      case (_, ConstantBottom) => x
      case (_, _) => if(x == y) x else ConstantTop
    }
  }

  /**
    * @inheritdoc
    */
  def glb(x : Constant, y : Constant) : Constant = {
    (x, y) match {
      case (ConstantBottom, _) => ConstantBottom
      case (_, ConstantBottom) => ConstantBottom
      case (ConstantTop, _) => y
      case (_, ConstantTop) => x
      case (_, _) => if(x == y) x else ConstantBottom
    }
  }

  /**
    * @inheritdoc
    */
  def compare(x : Constant, y : Constant) : Option[Int] = {
      (x, y) match {
      case (ConstantTop, ConstantTop) => Option(0)
      case (ConstantTop, _) => Option(1)
      case (_, ConstantTop) => Option(-1)
      case (ConstantBottom, ConstantBottom) => Option(0)
      case (ConstantBottom, _) => Option(-1)
      case (_, ConstantBottom) => Option(1)
      case (_, _) => if(x.equals(y)) Option(0) else Option.empty
    }
  }

  /**
    * @inheritdoc
    */
  override def top: Constant = ConstantTop

  /**
    * @inheritdoc
    */
  override def bottom: Constant = ConstantBottom
}

object ConstantDomainCore {
  /**
    * Factory method of ConstantDomainCore
    */
  def apply() = new ConstantDomainCore
}