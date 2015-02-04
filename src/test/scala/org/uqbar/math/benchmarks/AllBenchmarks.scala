package org.uqbar.math.benchmarks

object AllBenchmarks extends Benchmark {
  
  def operations = ReadWriteBenchmark.operations ++ SumBenchmark.operations
  
}