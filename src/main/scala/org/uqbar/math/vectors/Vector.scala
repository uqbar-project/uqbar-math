package org.uqbar.math.vectors

import scala.math.acos
import scala.math.sqrt
import org.uqbar.math.spaces.Axis
import scala.collection.mutable.AnyRefMap
import scala.collection.mutable.Map

trait Vector {

  def x: Double
  def y: Double
  //It is very convenient to have all vectors understand z
  def z: Double
  
  //*********************************************************************************************
  // QUERYING
  //*********************************************************************************************

  def apply(ax: Axis) = {
    ax.getFrom(this)
  }
  /**
   * Returns the list of the components of this vector, ordered by X, Y
   */
  def components: Seq[Double] = axes.map(_.getFrom(this))
  
  /**
   * Returns a map of the components
   */
  def componentsMap:Map[Axis, Double] = AnyRefMap(axes.zip(components):_*)

  def axes: Seq[Axis]

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
  def +(v: Vector): Vector = zipWith(_ + _)(v)
  /**
   * Returns the subtraction from this vector of the one received.
   */
  def -(v: Vector): Vector = zipWith(_ - _)(v)
  /**
   * Returns the dot product between this vector and the one received.
   */
  def °(v: Vector): Double = zipWith(_ * _)(v).flatten(_ + _)
  /**
   * Returns the scalar product between this vector and the scalar received.
   */
  def *(d: Double): Vector = map(_ * d)

  /**
   * Returns the scalar division between this vector and the scalar received.
   */
  def /(d: Double): Vector = map(_ / d)

  /**
   * Returns the module (or magnitude) of this vector.
   */
  def module = sqrt(dotSelf)

  /**
   * Applies dot product (°) to itself. Equivalent to this ° this
   */
  def dotSelf = this ° this

  /**
   * Returns a vector with the same direction as this, but magnitude 1.
   */
  def asVersor = this / module

  //*********************************************************************************************
  // HIGHER-ORDER OPERATIONS. These have to be redefined for every dimension added.
  //*********************************************************************************************

  /**
   * Returns the vector resultant of applying a function to every dimension value.
   */
  //These operations could be more "generic" if we used collections, but collections are very slow in comparison with variables
  def map(f: Double ⇒ Double): Vector = (f(x), f(y))

  /**
   * Returns a vector composed of the result of applying the given function to every dimension of booth this vector and the one received.
   */
  def zipWith(f: (Double, Double) ⇒ Double)(v: Vector): Vector = (f(x, v.x), f(y, v.y))

  /**
   * Operates over the components of this vector to produce a scalar
   */
  def flatten[T <% Double](f: (Double, Double) ⇒ T): T = f(x, y)

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
    (p - this).dotSelf
  }

  /**
   * Returns the Manhattan distance from this vector to the received one, booth interpreted as a points.
   */
  def manhattanDistanceTo(v: Vector) = (this - v).flatten(_.toInt.abs + _.toInt.abs)

  //*********************************************************************************************
  // PRINTING
  //*********************************************************************************************

  override def toString = (x, y).toString

  //*********************************************************************************************
  // CLONING
  //********************************************************************************************* 
  def copy: this.type
  
}

trait AbstractMutableVector extends Vector {
  def set(v: Vector): AbstractMutableVector = {
    set(v.x, v.y, v.z)
  }

  def set(someX: Double, someY: Double, someZ: Double = 0): AbstractMutableVector = {
    this.x = someX
    this.y = someY
    this.z = someZ
    this
  }
  
  def setAxis(ax: Axis, v: Double) = ax.setOn(this, v)

  def x_=(someX: Double): Unit
  def y_=(someY: Double): Unit
  def z_=(someZ: Double): Unit

  def operateWith(op: (Double, Double) ⇒ Double)(v: Vector) = set(op(x, v.x), op(y, v.y))

  def mapSet(op: Double ⇒ Double) = set(op(x), op(y))

  /**
   * Updates this vector to be equal to the result of the addition between itself and the one received.
   */
  def +=(v: Vector) = operateWith(_ + _)(v)

  /**
   * Updates this vector to be equal to the result of the subtraction between itself and the one received.
   */
  def -=(v: Vector) = operateWith(_ - _)(v)
  /**
   * Updates this vector to be equal to the result of the scalar product between itself and the scalar received.
   */
  def *=(d: Double) = mapSet(_ * d)
  /**
   * Updates this vector to be equal to the result of the scalar division between itself and the scalar received.
   */
  def /=(d: Double) = mapSet(_ / d)

}

case class MutableVector(var x: Double, var y: Double) extends AbstractMutableVector {
  /**
   * Sets this vector dimension values to the same ones as the vector given.
   */

  /**
   * On 2d Vectors, z is always 0
   */
  def z = 0;
  
  //Why array? Because benchmarks shows that it is created around three times faster than other sequences, and, when small, iterated around 50% faster
  def axes = Array(Axis.X, Axis.Y)

  /* NADA. Quizás conviene pensar un mecanismo por el cual un vector bidimensional
   * pueda convertirse en un vector tridimensional. La idea sería que ignore la componente z
   * hasta que se le asigne un valor. Si se lo piden explícitamente, el valor de la componente es 0,
   * por conveniencia, pero al sumar, restar, hacer dot product y todo eso, quiero ignorarlo.
   */
  def z_=(someZ: Double): Unit = {}

  def copy: this.type = MutableVector(x, y).asInstanceOf[this.type]
}

case class MutableVector3D(var x: Double, var y: Double, var z: Double) extends AbstractMutableVector {
  def axes = Array(Axis.X, Axis.Y, Axis.Z)
  
  override def toString = (x, y, z).toString

  override def map(f: Double ⇒ Double): Vector = (f(x), f(y), f(z))

  /**
   * Returns a vector composed of the result of applying the given function to every dimension of booth this vector and the one received.
   */
  override def zipWith(f: (Double, Double) ⇒ Double)(v: Vector): Vector = (f(x, v.x), f(y, v.y), f(z, v.z))

  /**
   * Operates over the components of this vector to produce a scalar
   */
  override def flatten[T <% Double](f: (Double, Double) ⇒ T): T = f(f(x, y), z)

  override def operateWith(op: (Double, Double) ⇒ Double)(v: Vector) = set(op(x, v.x), op(y, v.y), op(z, v.z))

  override def mapSet(op: Double ⇒ Double) = set(op(x), op(y), op(z))
  
  def copy: this.type = MutableVector3D(x, y, z).asInstanceOf[this.type]
}