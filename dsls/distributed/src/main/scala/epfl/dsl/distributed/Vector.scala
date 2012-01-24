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
      def apply[A : Manifest](file : Rep[String]) = vectorNew(file)
    }
    
    def infix_save[A : Manifest](vector : Rep[Vector[A]], path : Rep[String]) = save(vector, path)


    //operations
    def vectorNew[A:Manifest](file : Rep[String]): Rep[Vector[A]]
    def save[A : Manifest](vector : Rep[Vector[A]], path : Rep[String]) : Rep[Unit]
  
}

trait VectorOpsExp extends VectorOps with VariablesExp with BaseFatExp with DeliteCollectionOpsExp {
  this: SimpleVectorExp =>
    
    case class NewVector[A : Manifest](file : Exp[String]) extends Def[Vector[A]] {
      val mA = manifest[A]
    }
    case class VectorSave[A : Manifest](vector : Exp[Vector[A]], path : Rep[String]) extends Def[Unit] {
      val mA = manifest[A]
  }
    
    override def vectorNew[A: Manifest](file : Exp[String]) = reflectPure(NewVector[A](file))

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