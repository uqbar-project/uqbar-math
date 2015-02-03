package org.uqbar.math.spaces

import R2._

object SumBenchmark extends Benchmark {
  
  val v1:Vector = (3, 4)
  val v2:Vector = (5, 6)
  val v3 = org.uqbar.math.vectors.MutableVector(3, 4)
  val v4 = org.uqbar.math.vectors.MutableVector(5, 6)
  val v5 = new CachedVector(3,4)(new CachedSpace(2))
  val v6 = new CachedVector(5,6)(new CachedSpace(2))
  var x:Any = null

  def operations = Seq(
      ("New Sum", 
          () => x = v1 + v2    
      ),
      ("Cached Sum", 
          () => x = v5 + v6    
      ),
      ("Old Sum",
          () => x = v3 + v4
      )
  ) 
  
}