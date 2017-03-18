package it.unipd.jandom.domains.numerical.constant

object Constant {
  trait Constant
  // constant value
  case class Const (num : Int) extends Constant
  // no accurate info available for variable
  case object ConstantTop extends Constant
  // no possible value
  case object ConstantBottom extends Constant
}