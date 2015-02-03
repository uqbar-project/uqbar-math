package org.uqbar.math.spaces

import R2._

object ReadWriteBenchmark extends Benchmark {
  
  val v1:Vector = (3, 4)
  val v2 = org.uqbar.math.vectors.MutableVector(3, 4)
  val v3 = new CachedVector(3,4)(new CachedSpace(2))
  var x:Double = 0

  def operations = Seq(
      ("New read", 
          () => x = v1(X)    
      ),
      ("Cached read", 
          () => x = v3(X)    
      ),
      ("Old read",
          () => x = v2.x
      ),
      ("New write",
          () => v1.set(X, x)
      ),
      ("Cached write", 
          () => v3.set(X, x)    
      ),
      ("Old write",
          () => v2.x = x
      )
  ) 
}