package ppl.apps.ml.kmeans

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object kmeansRunner extends OptiMLApplicationRunner with kmeansApp

trait kmeansApp extends OptiMLApplication {

  def print_usage = {
    println("Usage: kmeans <input data file> <initmu data file>")
    exit(-1)
  }
  
  private val tol = 0.001 // tolerance (for convergence)
  private val k = 16 // num clusters

  def main() {
    
    if (args.length < 1) print_usage

    val x = readMatrix(args(0)) //TrainingSet(readMatrix(args(0)), Labels[Int](0))
    val mu = readMatrix(args(1))

    tic(mu)
    val (iter, mu2) = kmeans.cluster(x, k, tol, Some(mu))
    toc(mu2)
    println("finished in " + iter + " iterations")
    mu2.pprint

  }
}
