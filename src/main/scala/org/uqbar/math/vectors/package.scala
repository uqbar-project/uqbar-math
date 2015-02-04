package org.uqbar.math

import scala.language.implicitConversions
import java.awt.{ Point => AWTPoint, Dimension => AWTDimension }

package object vectors {
	val Origin: Vector = (0, 0)

	def random: Vector = (Math.random, Math.random)

	implicit def Touple_to_Vector[T <% Double, U <% Double](t: (T, U)): MutableVector = MutableVector(t._1, t._2)
  implicit def Touple_to_Vector3D[T <% Double, U <% Double, V <% Double](t: (T, U, V)): MutableVector3D = MutableVector3D(t._1, t._2, t._3)
	implicit def Vector_to_Touple(v: Vector): (Double, Double) = (v.x, v.y)

	implicit def Point_to_Vector(p: AWTPoint): Vector = (p.x, p.y)
	implicit def Vector_to_Point(v: Vector): AWTPoint = new AWTPoint(v.x.toInt, v.y.toInt)

	implicit def Dimension_to_Vector(d: AWTDimension): Vector = (d.width, d.height)
	implicit def Vector_to_Dimension(v: Vector): AWTDimension = new AWTDimension(v.x.toInt, v.y.toInt)

	implicit class ExtendedDouble(d: Double) {
		def *(v: Vector) = v * d
		def /(v: Vector) = v / d
	}

	implicit class ExtendedTouple[T <% Double, U <% Double](t: (T, U)) {
		def equals(v: Vector) = v == (t: Vector)
	}
}