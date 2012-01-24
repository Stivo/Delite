
package epfl.apps.distributed

import epfl.dsl.distributed.SimpleVectorApplication
import epfl.dsl.distributed.SimpleVectorApplicationRunner

object ToyRunner extends SimpleVectorApplicationRunner with Toy

trait Toy extends SimpleVectorApplication {

	def main() {
		val v = Vector[String]("asdf")
		v.save("asdf2")
		//    println(v)
	}

}