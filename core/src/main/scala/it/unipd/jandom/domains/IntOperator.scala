package it.unipd.jandom.domains

/**
  * Created by m0bius on 3/14/17.
  */
trait IntOperator[T] extends NumOperator[T] {
  def remainder(x : T, y : T) : T
}
