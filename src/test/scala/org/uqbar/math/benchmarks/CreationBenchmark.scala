package org.uqbar.math.benchmarks

import org.uqbar.math.spaces.R2._
import org.uqbar.math.spaces.CachedSpace
import org.uqbar.math.spaces.CachedVector

object CreationBenchmark extends Benchmark {
  
  val cachedSpace = new CachedSpace(2)
  var x:Any = null

  def operations = Seq(
      ("New Create", 
          () => x = vector(3,4)    
      ),
      ("Cached Create", 
          () => x = new CachedVector(5,6)(cachedSpace)    
      ),
      ("Old Create",
          () => x = org.uqbar.math.vectors.MutableVector(3, 4)
      )
  ) 
  
}