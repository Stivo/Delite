package ppl.delite.runtime.codegen

import java.util.ArrayDeque
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.scheduler.{StaticSchedule, PartialSchedule}
import ppl.delite.runtime.Config

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 9:41:08 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Compilers {

  def compileSchedule(graph: DeliteTaskGraph, startLocation: Int, endLocation: Int): StaticSchedule = {
    //generate executable(s) for all the ops in each proc
    //TODO: this is a poor method of separating CPU from GPU, should be encoded
    val numThreads = Config.numThreads
    val numGPUs = Config.numGPUs
    val schedule = graph.schedule.slice(startLocation, endLocation)
    assert((numThreads + numGPUs) == schedule.numResources)
    MainGenerator.makeExecutables(schedule.slice(startLocation, startLocation + numThreads))
    for (location <- startLocation + numThreads until startLocation + numThreads + numGPUs)
      GPUMainGenerator.makeExecutable(schedule.slice(location, location+1), location)

    if (Config.printSources) { //DEBUG option
      ScalaCompile.printSources()
      CudaCompile.printSources()
    }

    CudaCompile.compile()

    val classLoader = ScalaCompile.compile
    val queues = new Array[ArrayDeque[DeliteExecutable]](schedule.numResources)
    for (i <- 0 until schedule.numResources) {
      val cls = classLoader.loadClass(MainGenerator.className(i)) //load the Executable class
      val executable = cls.getMethod("self").invoke(null).asInstanceOf[DeliteExecutable] //retrieve the singleton instance

      queues(i) = new ArrayDeque[DeliteExecutable]
      queues(i).add(executable)
    }

    new StaticSchedule(queues)
  }

}
