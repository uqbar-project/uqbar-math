package org.uqbar.math.spaces

import scala.language.implicitConversions

trait AbstractSpace[T] {

  implicit def currentSpace = this;

  def axisList: Seq[Axis]

  def scalarZero: T

  def scalarPlus: (T, T) => T

  def scalarInverse: T => T

  def scalarByScalar: (T, T) => T

  def scalarDividedByScalar: (T, T) => T

  def scalarSqrt: T => T
  
  def acos: T => T

  def vector(values: T*) = GenericVector(values: _*)

  def origin: GenericVector[T] = vector(axisList.map { x => scalarZero }: _*)

  def plus(x: GenericVector[T], y: GenericVector[T]) = x.zipWith(scalarPlus)(y)

  def scalarProduct(x: GenericVector[T], s: T) = x.map(scalarByScalar(_: T, s))

  def scalarDivide(x: GenericVector[T], s: T) = x.map(scalarDividedByScalar(_: T, s))

  def defaultFor(ax: Axis) = scalarZero //Por defecto

  def module(x: GenericVector[T]) = scalarSqrt(x.dotSelf)

  def normed(x: GenericVector[T]) = x / x.module

  def inverse(x: GenericVector[T]) = x.map { scalarInverse }
  
  def dotProduct(x: GenericVector[T], y: GenericVector[T]) = x.zipComponentsWith(scalarByScalar)(y).reduce(scalarPlus)

}

//Debería ser posible definir un espacio genérico para tipos numéricos...
//SPAAAAACEEEEEE!
case class Space(val axisList: Axis*) extends AbstractSpace[Double] {
  override val scalarZero = 0.0
  override val scalarPlus = (x: Double, y: Double) => x + y
  override val scalarByScalar = (x: Double, y: Double) => x * y
  override val scalarInverse = (x: Double) => -x
  override val scalarSqrt = (x: Double) => Math.sqrt(x)
  //Esto podría definirse en función de elementByScalar, pero creo que es más óptimo así, y más claro también
  override val scalarDividedByScalar = (x: Double, y: Double) => x / y
  override val acos = (x: Double) => Math.acos(x)
}

trait SpaceContext[T] {
  implicit def space: AbstractSpace[T]
  def vector(values: T*) = space.vector(values: _*)
}

trait DoubleSpaceContext extends SpaceContext[Double] {
  type Vector = GenericVector[Double]
  
  implicit class ExtendedDouble(d: Double) {
    def *(v: GenericVector[Double]) = v * d
  }
}

object Spaces {
  object R2 extends DoubleSpaceContext {
    implicit def space = Space(Axis.X, Axis.Y)
    implicit def touple_to_vector[T <% Double, U <% Double](t: (T, U)): GenericVector[Double] = vector(t._1, t._2)

    implicit class ExtendedTouple[T <% Double, U <% Double](t: (T, U)) {
      def equals(v: GenericVector[Double]) = v == (touple_to_vector(t))
    }

  }

  object R3 extends DoubleSpaceContext {
    implicit def space = Space(Axis.X, Axis.Y, Axis.Z)
    implicit def touple_to_vector[T <% Double, U <% Double, V <% Double](t: (T, U, V)): GenericVector[Double] = vector(t._1, t._2, t._3)
    
    implicit class ExtendedTouple[T <% Double, U <% Double, V <% Double](t: (T, U, V)) {
      def equals(v: GenericVector[Double]) = v == (touple_to_vector(t))
    }
  }
}

