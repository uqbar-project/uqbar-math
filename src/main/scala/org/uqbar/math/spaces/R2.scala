package org.uqbar.math.spaces

import scala.language.implicitConversions

object R2 extends DoubleSpaceContext {
  val X = Axis.X
  val Y = Axis.Y
  val Origin = space.origin
  
  implicit def space = Space(Axis.X, Axis.Y)
  implicit def touple_to_vector[T <% Double, U <% Double](t: (T, U)): GenericVector[Double] = vector(t._1, t._2)

  implicit class ExtendedTouple[T <% Double, U <% Double](t: (T, U)) {
    def equals(v: GenericVector[Double]) = v == (touple_to_vector(t))
  }

}