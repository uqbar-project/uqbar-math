package org.uqbar.math.benchmarks

import scala.collection.mutable.AnyRefMap
import scala.collection.immutable.HashMap
import scala.collection.immutable.TreeMap

object CollectionCreation extends Benchmark {

  var x: Any = null

  def operations = Seq(
    ("Seq",
      () => x = Seq(1.0, 2.0, 3.0)),
    ("List",
      () => x = List(1.0, 2.0, 3.0)),
    ("Array",
      () => x = Array(1.0, 2.0, 3.0):Seq[Double]),
    ("Vector",
      () => x = Vector(1.0, 2.0, 3.0)),
    ("Map",
      () => x = Map(("x", 1.0), ("y", 2.0), ("z", 3.0))),
    ("Hashmap",
      () => x = HashMap(("x", 1.0), ("y", 2.0), ("z", 3.0))),
    ("Treemap",
      () => x = TreeMap(("x", 1.0), ("y", 2.0), ("z", 3.0))),
    ("AnyRefMap",
      () => x = AnyRefMap(("x", 1.0), ("y", 2.0), ("z", 3.0))) 
    )

}