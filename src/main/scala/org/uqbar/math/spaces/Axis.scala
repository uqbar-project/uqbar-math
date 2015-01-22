package org.uqbar.math.spaces

trait Axis {
  def apply[T](value: T)(implicit sp: AbstractSpace[T]) = (this, value)
}

object Axis {
  case object X extends Axis
  case object Y extends Axis
  case object Z extends Axis
}