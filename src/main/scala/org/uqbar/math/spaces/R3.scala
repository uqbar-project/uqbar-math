package org.uqbar.math.spaces

import scala.language.implicitConversions

object R3 extends DoubleSpaceContext {
  implicit def space = Space(Axis.X, Axis.Y, Axis.Z)
  implicit def touple_to_vector[T <% Double, U <% Double, V <% Double](t: (T, U, V)): GenericVector[Double] = vector(t._1, t._2, t._3)

  implicit class ExtendedTouple[T <% Double, U <% Double, V <% Double](t: (T, U, V)) {
    def equals(v: GenericVector[Double]) = v == (touple_to_vector(t))
  }
}