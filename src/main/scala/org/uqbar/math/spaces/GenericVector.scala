package org.uqbar.math.spaces

import scala.collection.mutable.LinkedHashMap

//TODO: La clase component puede ser útil, pero creo que va a ser mejor como un caso excepcional, y no como lo normal. Podría hacerse un método que devuelva el "component"
//case class Component[T](axis: Axis, value: T)(implicit sp: AbstractSpace[T]) {
//  def +(component: Component[T]) = Component(axis, sp.plusElement(this.value, component.value))
//
//  def *(scalar: T) = Component(axis, sp.elementByScalar(this.value, scalar))
//  
//  def *(component)
//  
//  def unary_- = Component(axis, sp.inverseElement(value))
//}

case class GenericVector[T](someComponents: T*)(implicit sp: AbstractSpace[T]) {
  
  val componentMap = LinkedHashMap(sp.axisList.zip(someComponents):_*).withDefault { x => sp.defaultFor(x) }
  
  def components = componentMap.values.toSeq

  def +(x: GenericVector[T]) = sp.plus(this, x)
  
  def -(x: GenericVector[T]) = this + (-x)
  
  def °(x: GenericVector[T]) = sp.dotProduct(this, x) 

  def *(s: T) = sp.scalarProduct(this, s)
  
  def /(s: T) = sp.scalarDivide(this, s)
  
  def zipComponentsWith[B, C](f:(T, B) => C)(x:GenericVector[B]) =
    this.components.zip(x.components).map(f.tupled)
  
  def zipWith[B, C](f:(T, B) => C)(x:GenericVector[B])(implicit foreignSp: AbstractSpace[C]) = 
    foreignSp.vector(this.zipComponentsWith(f)(x):_*)
  
  def map[B](f:T => B)(implicit foreingSp: AbstractSpace[B]) = 
    foreingSp.vector(components.map(f):_*)
  
  def flatMap[B](f:T => GenericVector[B])(implicit foreingSp: AbstractSpace[B]) = 
    foreingSp.vector((components.flatMap(f.andThen { _.components })):_*)

  def dotSelf = this ° this   
    
  def module = sp.module(this)
  
  def unary_- = sp.inverse(this) 
  
  def normed = sp.normed(this)
 
  def asVersor = normed
  
  def angleTo(v: GenericVector[T]):T = sp.acos(this.asVersor ° v.asVersor)
  
  def distanceTo(v: GenericVector[T]):T = (v - this).module
  
  def squareDistanceTo(v: GenericVector[T]):T = (v - this).dotSelf

  def apply(ax: Axis) = componentMap(ax)
  
  override def toString:String = componentMap.toString().replace("Map", "Vector")
  
  def set(someComponents:T *):Unit = someComponents.zipWithIndex.foreach { case (c, i) => 
    componentMap.update(sp.axisList(i), c)
  }
  
  def set(v:GenericVector[T]):Unit = componentMap.++=(v.componentMap)
}
