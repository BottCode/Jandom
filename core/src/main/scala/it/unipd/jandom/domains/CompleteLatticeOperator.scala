package it.unipd.jandom.domains

/**
  * Created by m0bius on 3/14/17.
  */
trait CompleteLatticeOperator [T] {
  def lub(x : T, y : T) : T  
  def glb(x : T, y : T) : T
  def compare(x : T, y : T) : Option[Int]
  def top : T
  def bottom : T
}
