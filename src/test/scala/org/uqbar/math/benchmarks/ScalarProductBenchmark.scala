package org.uqbar.math.benchmarks

import org.uqbar.math.spaces.R2._
import org.uqbar.math.spaces.CachedVector
import org.uqbar.math.spaces.CachedSpace

object ScalarProductBenchmark extends Benchmark {
  
  val v1:Vector = (3, 4)
  val v3 = org.uqbar.math.vectors.MutableVector(3, 4)
  val v5 = new CachedVector(3,4)(new CachedSpace(2))
  var x:Any = null

  def operations = Seq(
      ("New Scalar Product", 
          () => x = v1 * 3    
      ),
      ("Cached Scalar Product", 
          () => x = v5 * 3  
      ),
      ("Old Scalar Product",
          () => x = v3 * 3
      )
  ) 
  
}