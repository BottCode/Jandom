package it.unipd.jandom.domains

/**
  * @author $assume
  */
trait NumOperator [T] extends UniPDAnnotation {
  def inverse(x : T) : T
  def sum(x : T, y : T) : T
  def mult(x : T, y : T) : T
  def division(x : T, y : T) : T
}
