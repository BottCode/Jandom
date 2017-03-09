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

  def inverse(x: Constant) : Constant = {
    x match {
      case ConstantTop => ConstantTop
      case ConstantBottom => ConstantBottom
      case Constant(a) => Constant(-a)
    }
  }

  def lub(x : Constant, y : Constant) : Constant = {
    (x, y) match {
      case (ConstantTop, _) => ConstantTop
      case (_, ConstantTop) => ConstantTop
      case (ConstantBottom, a) => a
      case (a, ConstantBottom) => a
      case (a, b) => if(a == b) a else ConstantTop
    }
  }

  def glb(x : Constant, y : Constant) : Constant = {
    (x, y) match {
      case (ConstantBottom, _) => ConstantBottom
      case (_, ConstantBottom) => ConstantBottom
      case (ConstantTop, a) => a
      case (a, ConstantTop) => a
      case (a, b) => if(a == b) a else ConstantBottom
    }
  }

  def sum(x : Constant, y : Constant) : Constant = {
    (x,y) match {
      case (ConstantBottom, _) => ConstantBottom
      case (_, ConstantBottom) => ConstantBottom
      case (ConstantTop, _) => ConstantTop
      case (_, ConstantTop) => ConstantTop
      case (a, b) => a+b
    }
  }

  def mult(x : Constant, y : Constant) : Constant = {
    (x, y) match {
      case (ConstantBottom, _) => ConstantBottom
      case (_, ConstantBottom) => ConstantBottom
      case (Constant(0), _) => x
      case (_, Constant(0)) => y
      case (ConstantTop, _) => ConstantTop
      case (_, ConstantTop) => ConstantTop
      case (a, b) => a * b
    }
  }

  def division(x : Constant, y : Constant) : Constant = {
    (x, y) match {
      case (ConstantBottom, _) => ConstantBottom
      case (_, ConstantBottom) => ConstantBottom
      case (_, Constant(0)) => ConstantBottom
      case (ConstantTop, _) => ConstantTop
      case (_, ConstantTop) => ConstantTop
      case (a, b) => a/b
    }
  }

  /**
    * @note Java convention : the sign of the result is the sign of the dividend
    */
  def remainder(x : Constant, y : Constant) : Constant = {
      (x, y) match {
        case (ConstantBottom, _) => ConstantBottom
        case (_, ConstantBottom) => ConstantBottom
        case (_, ConstantTop) => ConstantTop
        case (_, Constant(0)) => ConstantBottom
        case (ConstantTop, _) => ConstantTop
        case (a, b) => a % b
      }
    }

  /**
    * @return Option(0) : x == y
    *         Option(1) : x > y
    *         Option(-1) : x < y
    */
  def compare(x : Constant, y : Constant) : Option[Int] = {
      (x, y) match {
      case (ConstantTop, ConstantTop) => Option(0)
      case (ConstantTop, _) => Option(1)
      case (_, ConstantTop) => Option(-1)
      case (ConstantBottom, ConstantBottom) => Option(0)
      case (ConstantBottom, _) => Option(-1)
      case (_, ConstantBottom) => Option(1)
      case (a, b) => if(a.equals(b)) Option(0) else Option.empty
    }
  }
}
