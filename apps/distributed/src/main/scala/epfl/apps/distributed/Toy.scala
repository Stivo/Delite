
package epfl.apps.distributed

import epfl.dsl.distributed.SimpleVectorApplication
import epfl.dsl.distributed.SimpleVectorApplicationRunner

object ToyRunner extends SimpleVectorApplicationRunner with Toy

trait Toy extends SimpleVectorApplication {
  
	def main() {
		val x1 = Vector[Int]("fffff")
		val v = Vector[Int]("asdf")
		//println(v(2))
		val v2 = v.map(_+3)
		val v3 = v2.map(_*2).map(_+1)
		//val v3 = v.map( x=> (x,x+1))
		//val v4 = v2.reduceByKey(_+_)
		//val v3 = v.map(_+5).filter(_>3)
		v3.save("asdf2")
		//v3.save("asf3")
		//    println(v)
	}

}
