package org.uqbar.math.spaces

import scala.reflect.ClassTag

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
  
  implicit def classTag: ClassTag[T]
  
  def vector(values: T*) = new GenericVector(values.toArray)
  
  def vector(values: Array[T]) = new GenericVector(values)

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
class Space(val axisList: Axis*) extends AbstractSpace[Double] {
  
  def classTag = ClassTag[Double](classOf[Double])
  
  override val scalarZero = 0.0
  override val scalarPlus = (x: Double, y: Double) => x + y
  override val scalarByScalar = (x: Double, y: Double) => x * y
  override val scalarInverse = (x: Double) => -x
  override val scalarSqrt = (x: Double) => Math.sqrt(x)
  //Esto podría definirse en función de elementByScalar, pero creo que es más óptimo así, y más claro también
  override val scalarDividedByScalar = (x: Double, y: Double) => x / y
  override val acos = (x: Double) => Math.acos(x)
}

/**
 * Por ahora sólo soporta 2 o 3 dimensiones. Hay que ver si se puede mejorar esto, sin sacrificar mucha performance
 * La idea es que el chequeador de tipos se asegure que el espacio proporcionado es correcto.
 */
class CachedSpace(dimensions: Int) extends Space((if(dimensions == 2) Seq(Axis.X, Axis.Y) else Seq(Axis.X, Axis.Y, Axis.Z)):_*) 

trait SpaceContext[T] {
  implicit def space: AbstractSpace[T]
  def vector(values: T*) = space.vector(values: _*)
  def vector(values: Array[T]) = space.vector(values)
}

trait DoubleSpaceContext extends SpaceContext[Double] {
  type Vector = GenericVector[Double]
  
  implicit class ExtendedDouble(d: Double) {
    def *(v: GenericVector[Double]) = v * d
  }
}
