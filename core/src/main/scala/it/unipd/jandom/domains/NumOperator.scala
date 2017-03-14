package it.unipd.jandom.domains

/**
  * Created by m0bius on 3/14/17.
  */
trait NumOperator [T] {
  def inverse(x : T) : T
  def sum(x : T, y : T) : T
  def mult(x : T, y : T) : T
  def division(x : T, y : T) : T
}
