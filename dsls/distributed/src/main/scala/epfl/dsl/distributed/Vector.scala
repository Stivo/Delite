package epfl.dsl.distributed

import virtualization.lms.common.{Variables, VariablesExp, BaseFatExp}
import ppl.delite.framework.ops._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.datastructures._
import java.io.PrintWriter

/**
 * Operations
 */

trait Vector[A] extends DeliteCollection[A] {
  //val elementType : Class[A]
}

trait VectorOps extends Variables {
  this: SimpleVector =>
	//syntax
    object Vector {
      def apply[A : Manifest](file : Rep[String]) = vector_new(file)
    }
    implicit def repVecToVecOps[A:Manifest](x: Rep[Vector[A]]) = new vecOpsCls(x)
    implicit def repVecToVecTupleOps[K: Manifest, V : Manifest](x: Rep[Vector[(K,V)]]) = new vecTupleOpsCls(x)
    class vecOpsCls[A:Manifest](x: Rep[Vector[A]]) {
    	def map[B:Manifest](f: Rep[A] => Rep[B]) = vector_map(x,f)
		def filter(f: Rep[A] => Rep[Boolean]) = vector_filter(x,f)
    }

    class vecTupleOpsCls[K: Manifest, V : Manifest](x: Rep[Vector[(K,V)]]) {
      def reduceByKey(f : (Rep[V], Rep[V]) => Rep[V] ) = vector_reduceByKey[K, V](x, f)
    }

    
    def infix_save[A : Manifest](vector : Rep[Vector[A]], path : Rep[String]) = save(vector, path)

    //operations
    def vector_new[A:Manifest](file : Rep[String]): Rep[Vector[A]]
    def vector_map[A : Manifest, B : Manifest](vector : Rep[Vector[A]], f : Rep[A] => Rep[B]) : Rep[Vector[B]]
    def vector_filter[A : Manifest](vector : Rep[Vector[A]], f: Rep[A] => Rep[Boolean]) : Rep[Vector[A]]
    def save[A : Manifest](vector : Rep[Vector[A]], path : Rep[String]) : Rep[Unit]
    def vector_reduceByKey[K: Manifest, V : Manifest](vector : Rep[Vector[(K,V)]], f : (Rep[V], Rep[V]) => Rep[V] ) : Rep[Vector[(K, V)]]
}

trait VectorOpsExp extends VectorOps with VariablesExp with BaseFatExp with DeliteCollectionOpsExp {
  this: SimpleVectorExp =>
    
    case class NewVector[A : Manifest](file : Exp[String]) extends Def[Vector[A]] {
      val mA = manifest[A]
    }
    
    case class VectorMap[A : Manifest, B : Manifest](vector : Exp[Vector[A]], f : Exp[A] => Exp[B]) extends Def[Vector[B]] {
      val mA = manifest[A]
      val mB = manifest[B]
    }

	case class VectorFilter[A : Manifest](vector : Exp[Vector[A]], f : Exp[A] => Exp[Boolean]) extends Def[Vector[A]] {
      val mA = manifest[A]
    }

    case class VectorSave[A : Manifest](vector : Exp[Vector[A]], path : Rep[String]) extends Def[Unit] {
      val mA = manifest[A]
    }
    
    case class VectorReduceByKey[K: Manifest, V : Manifest](vector : Exp[Vector[(K,V)]], f : (Exp[V], Exp[V]) => Exp[V]) extends Def[Vector[(K, V)]]
    
    override def vector_new[A: Manifest](file : Exp[String]) = reflectPure(NewVector[A](file))
    override def vector_map[A : Manifest, B : Manifest](vector : Exp[Vector[A]], f : Exp[A] => Exp[B]) = reflectPure(VectorMap[A, B](vector, f))
	override def vector_filter[A : Manifest](vector : Exp[Vector[A]], f: Exp[A] => Exp[Boolean]) = reflectPure(VectorFilter[A](vector, f))
    override def save[A : Manifest](vector : Exp[Vector[A]], path : Exp[String]) = reflectEffect(VectorSave[A](vector, path)) //, manifest[A].erasure.asInstanceOf[Class[A]]))
    override def vector_reduceByKey[K: Manifest, V : Manifest](vector : Exp[Vector[(K,V)]], f : (Exp[V], Exp[V]) => Exp[V] ) = reflectPure(VectorReduceByKey(vector, f))
}

/**
 * Implementation using kernel embedding
 */
trait VectorImplOps { this: SimpleVector =>
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: SimpleVectorCompiler with SimpleVectorLift =>

}
