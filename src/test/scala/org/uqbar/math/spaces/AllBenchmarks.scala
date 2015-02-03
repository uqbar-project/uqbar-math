package org.uqbar.math.spaces

object AllBenchmarks extends Benchmark {
  
  def operations = ReadWriteBenchmark.operations ++ SumBenchmark.operations
  
}