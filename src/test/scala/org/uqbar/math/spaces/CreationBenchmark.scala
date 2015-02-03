package org.uqbar.math.spaces

import R2._

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