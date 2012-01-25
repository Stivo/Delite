
package epfl.apps.distributed

import epfl.dsl.distributed.SimpleVectorApplication
import epfl.dsl.distributed.SimpleVectorApplicationRunner

object ToyRunner extends SimpleVectorApplicationRunner with Toy

trait Toy extends SimpleVectorApplication {
  
	def main() {
		val x1 = Vector[Int]("fffff")
		val v = Vector[Int]("asdf")
		val v2 = v.map(_+3).filter(_>4).map( x=> (x,x+1))
		val v4 = v2.reduceByKey(_+_)
		val v3 = v.map(_+5).filter(_>3)
		v4.save("asdf2")
		//    println(v)
	}

}
