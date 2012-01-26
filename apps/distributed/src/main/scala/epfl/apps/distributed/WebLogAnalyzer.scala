package epfl.apps.distributed

import epfl.dsl.distributed.SimpleVectorApplication
import epfl.dsl.distributed.SimpleVectorApplicationRunner

object WebLogAnalyzerRunner extends SimpleVectorApplicationRunner with WebLogAnalyzer

trait WebLogAnalyzer extends SimpleVectorApplication {
  
	def main() {
	  val input = Vector.apply[String]("/var/log/apache2/access.log")
      val fullLines = input.map(_.trim).filter(!_.isEmpty)
      val restLines = fullLines.map(_.trim).filter(_.contains("/2012:")).filter(_.startsWith("127."))
    	.map(_.split("\\]\\s",2))//.filter(_.contains("gitweb"))
      restLines.save("output")

	}
}