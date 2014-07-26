package org.uqbar.math.vectors

import scala.math.acos
import scala.math.sqrt

trait Vector {

	def x: Double
	def y: Double

	//*********************************************************************************************
	// BASIC OPERATIONS
	//*********************************************************************************************

	/**
	 * Returns the inverse of this vector.
	 *
	 * The inverse of a vector is another vector with the same magnitude but opposite direction.
	 * This method is equivalent to evaluating this * -1.
	 */
	def unary_- = this * -1

	/**
	 * Returns the addition of this vector and the one received.
	 */
	def +(v: Vector): Vector = (x + v.x, y + v.y)
	/**
	 * Returns the subtraction from this vector of the one received.
	 */
	def -(v: Vector): Vector = (x - v.x, y - v.y)
	/**
	 * Returns the dot product between this vector and the one received.
	 */
	def °(v: Vector): Double = x * v.x + y * v.y
	/**
	 * Returns the scalar product between this vector and the scalar received.
	 */
	def *(d: Double): Vector = (x * d, y * d)
	/**
	 * Returns the scalar division between this vector and the scalar received.
	 */
	def /(d: Double) = (x / d, y / d)

	/**
	 * Returns the module (or magnitude) of this vector.
	 */
	def module = sqrt(x * x + y * y)

	/**
	 * Returns a vector with the same direction as this, but magnitude 1.
	 */
	def asVersor = this / module

	//*********************************************************************************************
	// HIGHER-ORDER OPERATIONS
	//*********************************************************************************************

	/**
	 * Returns the vector resultant of applying a function to every dimension value.
	 */
	def map(f: Double ⇒ Double): Vector = (f(x), f(y))
	/**
	 * Returns a vector composed of the result of applying the given function to every dimension of booth this vector and the one received.
	 */
	def zipWith(f: (Double, Double) ⇒ Double)(v: Vector): Vector = (f(x, v.x), f(y, v.y))

	//*********************************************************************************************
	// COMPARATIONS
	//*********************************************************************************************

	/**
	 * Returns the angle between this vector and the one received.
	 */
	def angleTo(v: Vector) = acos(asVersor ° v.asVersor)

	/**
	 * Returns a vector composed of the minimum value for every dimension between this vector and the one received.
	 */
	def min(v: Vector) = zipWith(_ min _)(v)
	/**
	 * Returns a vector composed of the minimum value for every dimension between this vector and the one received.
	 */
	def max(v: Vector) = zipWith(_ max _)(v)

	//*********************************************************************************************
	// DISTANCE
	//*********************************************************************************************

	/**
	 * Returns the distance from this vector to the received one, booth interpreted as a points.
	 */
	def distanceTo(v: Vector) = (this - v).module

	/**
	 * Returns the square of the distance from this vector to the received one, booth interpreted as a points.
	 */
	def squareDistanceTo(p: Vector) = {
		val v = (p - this)
		v.x * v.x + v.y * v.y
	}

	/**
	 * Returns the Manhattan distance from this vector to the received one, booth interpreted as a points.
	 */
	def manhattanDistanceTo(v: Vector) = (x - v.x).toInt.abs + (y - v.y).toInt.abs

	//*********************************************************************************************
	// PRINTING
	//*********************************************************************************************

	override def toString = (x, y).toString
}

case class MutableVector(var x: Double, var y: Double) extends Vector {
	/**
	 * Sets this vector dimension values to the same ones as the vector given.
	 */
	def set(v: Vector) = {
		this.x = x
		this.y = y
		this
	}

	/**
	 * Updates this vector to be equal to the result of the addition between itself and the one received.
	 */
	def +=(v: Vector) = set(x + v.x, y + v.y)
	/**
	 * Updates this vector to be equal to the result of the subtraction between itself and the one received.
	 */
	def -=(v: Vector) = set(x - v.x, y - v.y)
	/**
	 * Updates this vector to be equal to the result of the scalar product between itself and the scalar received.
	 */
	def *=(d: Double) = set(x * d, y * d)
	/**
	 * Updates this vector to be equal to the result of the scalar division between itself and the scalar received.
	 */
	def /=(d: Double) = set(x / d, y / d)

}