package ppl.apps.ml

import ppl.dsl.optiml.OptiMLApplicationRunner
import ppl.dsl.optiml.OptiMLApplication

object ToyRunner extends OptiMLApplicationRunner with Toy

trait Toy extends OptiMLApplication {
  
	def main() {
		val v = readVector("asdf")
		//val v = Vector[Int]("asdf")
		//println(v(2))
		val v2 = v.map(_+3.toDouble).map( x=> (x,x+13.toDouble))
		//val v4 = v2.reduceByKey(_+_)
		//val v3 = v.map(_+5).filter(_>3)
		println(v2(4))
		//    println(v)
	}

}
