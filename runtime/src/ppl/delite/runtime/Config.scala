package ppl.delite.runtime

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:43:14 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Config {

  val numThreads: Int = System.getProperty("delite.threads", "1").toInt

  val numGPUs: Int = System.getProperty("delite.gpus", "0").toInt

  val numNodes: Int = System.getProperty("delite.nodes", "1").toInt

  val queueSize: Int = System.getProperty("delite.debug.queue.size", "128").toInt

  val scheduler: String = System.getProperty("delite.scheduler", "default")

  val executor: String = System.getProperty("delite.executor", "default")

  val printSources: Boolean = if (System.getProperty("delite.debug.print.sources") == null) false else true

  val numRuns: Int = System.getProperty("delite.runs", "1").toInt

  val deliteHome: String = System.getProperty("delite.home", System.getProperty("user.dir"))

  val codeCacheHome: String = System.getProperty("delite.code.cache.home", deliteHome + java.io.File.separator + "generatedCache")

  val useFsc: Boolean = if (System.getProperty("delite.usefsc") == null) false else true

  val noRegenerate: Boolean = if (System.getProperty("delite.debug.noregenerate") == null) false else true

  /***********
   *	Cost Modeling
   */
  
	val whileCostThreshold: Int = System.getProperty("delite.while.threshold", "-1").toInt
	val loopCostThreshold: Int = System.getProperty("delite.loop.threshold", "-1").toInt
	 
  /***********
    * Statistics and Metrics Section
    */
   val dumpStats: Boolean = if(System.getProperty("stats.dump") == null) false else true

   val dumpStatsComponent: String = System.getProperty("stats.dump.component", "all")

   val dumpStatsOverwrite: Boolean = if(System.getProperty("stats.dump.overwrite")== null) false else true

   val statsOutputDirectory: String = System.getProperty("stats.output.dir")
   if(dumpStats && statsOutputDirectory == null) throw new RuntimeException("stats.dump option enabled but did not provide a statsOutputDirectory")

   val statsOutputFilename: String = System.getProperty("stats.output.filename")
   if(dumpStats && statsOutputFilename == null) throw new RuntimeException("stats.dump option enabled but did not provide a statsOutputFilename")
  

}

