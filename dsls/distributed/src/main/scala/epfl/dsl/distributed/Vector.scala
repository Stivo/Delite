package epfl.dsl.distributed

import virtualization.lms.common.{Variables, VariablesExp, BaseFatExp}
import _root_.ppl.delite.framework.ops._
import _root_.ppl.delite.framework.datastruct.scala.DeliteCollection
import _root_.ppl.delite.framework.datastructures._
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
 
    class vecOpsCls[A:Manifest](x: Rep[Vector[A]]) {
    	def map[B:Manifest](f: Rep[A] => Rep[B]) = vector_map(x,f)
		def filter(f: Rep[A] => Rep[Boolean]) = vector_filter(x,f)
    }
    
    def infix_save[A : Manifest](vector : Rep[Vector[A]], path : Rep[String]) = save(vector, path)
	//def infix_head[A : Manifest](vector : Rep[Vector[A]]) = head(vector)
    //def infix_map[A : Manifest](vector : Rep[Vector[A]], f : Rep[A] => Rep[String]) = map[A, String](vector, f)

    //operations
    def vector_new[A:Manifest](file : Rep[String]): Rep[Vector[A]]
    def vector_map[A : Manifest, B : Manifest](vector : Rep[Vector[A]], f : Rep[A] => Rep[B]) : Rep[Vector[B]]
    def vector_filter[A : Manifest](vector : Rep[Vector[A]], f: Rep[A] => Rep[Boolean]) : Rep[Vector[A]]
    def save[A : Manifest](vector : Rep[Vector[A]], path : Rep[String]) : Rep[Unit]
  
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
    
    override def vector_new[A: Manifest](file : Exp[String]) = reflectEffect(NewVector[A](file))

    override def vector_map[A : Manifest, B : Manifest](vector : Exp[Vector[A]], f : Exp[A] => Exp[B]) = reflectPure(VectorMap[A, B](vector, f))

	override def vector_filter[A : Manifest](vector : Exp[Vector[A]], f: Exp[A] => Exp[Boolean]) = reflectPure(VectorFilter[A](vector, f))
   
    override def save[A : Manifest](vector : Exp[Vector[A]], path : Exp[String]) = reflectEffect(VectorSave[A](vector, path)) //, manifest[A].erasure.asInstanceOf[Class[A]]))
    
}

/**
 * Implementation using kernel embedding
 */
trait VectorImplOps { this: SimpleVector =>
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: SimpleVectorCompiler with SimpleVectorLift =>

}
