package org.uqbar.math

import scala.language.implicitConversions

package object vectors {
	val Origin: Vector = (0, 0)

	implicit def touple_to_vector[T <% Double, U <% Double](t: (T, U)): MutableVector = MutableVector(t._1, t._2)

	implicit class ExtendedDouble(d: Double) {
		def *(v: Vector) = v * d
		def /(v: Vector) = v / d
	}

	implicit class ExtendedTouple[T <% Double, U <% Double](t: (T, U)) {
		def equals(v: Vector) = v == (t: Vector)
	}
}