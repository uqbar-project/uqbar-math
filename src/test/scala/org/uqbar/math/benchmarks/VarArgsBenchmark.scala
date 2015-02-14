package org.uqbar.math.benchmarks

object VarArgsBenchmark extends Benchmark {
  
  def sumVarArgs(args:Int*) = args(0) + args(1) + args(2)
  def sumNormalArgs(x: Int, y:Int, z:Int) = x + y + z
  def sumArray(args:Array[Int]) = args(0) + args(1) + args(2)

  def operations = Seq(
      ("Var Args", 
          () => sumVarArgs(1,2,3)    
      ),
      ("Normal Args", 
          () => sumNormalArgs(1,2,3)    
      ),
      ("Array args",
          () => sumArray(Array(1,2,3))
      )
  ) 
  
}