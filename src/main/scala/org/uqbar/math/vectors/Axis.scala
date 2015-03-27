package org.uqbar.math.vectors

trait Axis {
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