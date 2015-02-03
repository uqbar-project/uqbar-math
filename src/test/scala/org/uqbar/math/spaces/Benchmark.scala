package org.uqbar.math.spaces

import scala.collection.mutable.ArrayBuffer

trait Benchmark {

  def operations: Seq[(String, () => Unit)]

  /**
   * Tiempo durante el cual se repetirá la operación en milisegundos
   */
  def operationTime = 5000l

  /**
   * Tiempo adicional en milisegundos durante el cual se repetirá la operación, pero que no se considerará en las estadísticas finales
   */
  def warmUpTime = 1000l

  def runBenchmark(title: String, op: () => Unit) = {
    println(s"Running $title")
    println("Warming up...")
    warmUp(op)

    println(s"Running benchmark...")
    val runs = runFor(operationTime, op)
    println(s"Benchmark finished. Total runtime: ${runs._2} millis")
    println(f"Total Runs: ${runs._1}%d (${runs._1.toDouble / runs._2 * 1000}%.4f runs per second)")
    
    (title, runs)
  }

  def warmUp(op: () => Unit) = {
    runFor(warmUpTime, op)
  }

  def runFor(runningTime: Long, op: () => Unit) = {
    val startTime = System.nanoTime
    val runningTimeNano = runningTime * 1000 * 1000
    
    var runs = 0l
    var finalRunningTime = 0l
    while(finalRunningTime < runningTimeNano) {
      op()
      runs+=1
      finalRunningTime = System.nanoTime - startTime
    }
      
    (runs, finalRunningTime.toDouble / 1000000)
  }
  
  def runBenchmarks = {
    val results = ArrayBuffer[(String, (Long, Double))]()
    for ((title, op) <- operations)
      results += runBenchmark(title, op)
      
    print("\n-------------------------------\nSUMMARY\n-------------------------------\n");
    
    print(f"\n${"Operation"}%-20s | ${"Runs"}%-20s | ${"Runs/s"}%-20s\n")
    println("-" * 60)  
    for( (title, (runs, runningTime)) <- results)
      println(f"$title%-20s | ${runs}%-20d | ${runs.toDouble/runningTime * 1000}%-20.4f"); 
  }

  
  def main(args: Array[String]) = {
    runBenchmarks
  }
}