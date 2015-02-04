package org.uqbar.math.spaces

import org.uqbar.math.vectors.Vector
import org.uqbar.math.vectors.AbstractMutableVector

trait Axis {
  def apply[T](value: T)(implicit sp: AbstractSpace[T]) = (this, value)
  def getFrom(v:Vector): Double
  def setOn(arg: AbstractMutableVector, v: Double)
}

object Axis {
  
  case object X extends Axis {
    def getFrom(v:Vector) = v.x
    def setOn(arg: AbstractMutableVector, v: Double) = {
      arg.x = v
    }
  }
  case object Y extends Axis {
    def getFrom(v:Vector) = v.y
    def setOn(arg: AbstractMutableVector, v: Double) = {
      arg.y = v
    }
  }
  case object Z extends Axis {
    def getFrom(v:Vector) = v.z
    def setOn(arg: AbstractMutableVector, v: Double) = {
      arg.z = v
    }
  }
  
  val axes = Array(X, Y, Z)
}