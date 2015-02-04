package org.uqbar.math.spaces

import org.uqbar.math.vectors.Vector

trait Axis {
  def apply[T](value: T)(implicit sp: AbstractSpace[T]) = (this, value)
  def getFrom(v:Vector): Double
}

object Axis {
  
  case object X extends Axis {
    def getFrom(v:Vector) = v.x
  }
  case object Y extends Axis {
    def getFrom(v:Vector) = v.y
  }
  case object Z extends Axis {
    def getFrom(v:Vector) = v.z
  }
  
  val axes = Array(X, Y, Z)
}