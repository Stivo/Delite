
package epfl.apps.distributed

import epfl.dsl.distributed.SimpleVectorApplication
import epfl.dsl.distributed.SimpleVectorApplicationRunner

object ToyRunner extends SimpleVectorApplicationRunner with Toy

trait Toy extends SimpleVectorApplication {
  
	def main() {
		val v = Vector[Int]("asdf")
		val v2 = v.map(_+3).filter(_>4)
		val v3 = v.map(_+5).filter(_>3)
		v2.save("asdf2")
		//    println(v)
	}

}
