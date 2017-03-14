package it.unipd.jandom.domains

/**
  * @define UniPDAUTHORS  Mirko Bez, Stefano Munari, Sebastiano Valle
  */
trait CompleteLatticeOperator [T] {
  def lub(x : T, y : T) : T  
  def glb(x : T, y : T) : T
  def compare(x : T, y : T) : Option[Int]
  def top : T
  def bottom : T
}
