package org.uqbar.math.spaces

//Esto todavía no hace nada. Es una idea
trait Direction {
  def axis: Axis
}

object Direction {
  case object Left extends Direction { val axis = Axis.X }
  case object Right extends Direction { val axis = Axis.X }
  case object Up extends Direction { val axis = Axis.Y }
  case object Down extends Direction { val axis = Axis.Y }
  case object Front extends Direction { val axis = Axis.Z }
  case object Back extends Direction { val axis = Axis.Z }
}