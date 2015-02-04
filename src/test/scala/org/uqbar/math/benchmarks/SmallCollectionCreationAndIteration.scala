package org.uqbar.math.benchmarks

import scala.collection.mutable.AnyRefMap
import scala.collection.immutable.HashMap
import scala.collection.immutable.TreeMap

object SmallCollectionCreationAndIteration extends Benchmark {

  var x: Any = null

  def operations = Seq(
    ("Seq",
      () => x = Seq(1.0, 2.0, 3.0).map(_*2)),
    ("List",
      () => x = List(1.0, 2.0, 3.0).map(_*2)),
    ("Array",
      () => x = Array(1.0, 2.0, 3.0).map(_*2)),
    ("Vector",
      () => x = Vector(1.0, 2.0, 3.0).map(_*2))
    )
}