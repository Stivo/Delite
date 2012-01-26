package epfl.dsl.distributed

import virtualization.lms.common.{Variables, VariablesExp, BaseFatExp}
import ppl.delite.framework.ops._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.datastructures._
import java.io.PrintWriter
import scala.reflect.SourceContext

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
    implicit def repVecToVecOps[A:Manifest](vector: Rep[Vector[A]]) = new vecOpsCls(vector)
    implicit def repVecToVecTupleOps[K: Manifest, V : Manifest](x: Rep[Vector[(K,V)]]) = new vecTupleOpsCls(x)
    class vecOpsCls[A:Manifest](vector: Rep[Vector[A]]) {
    	def map[B:Manifest](f: Rep[A] => Rep[B]) = vector_map(vector,f)
		def filter(f: Rep[A] => Rep[Boolean]) = vector_filter(vector,f)
		def save[A : Manifest](path : Rep[String]) = vector_save(vector, path)
    }

    class vecTupleOpsCls[K: Manifest, V : Manifest](x: Rep[Vector[(K,V)]]) {
      def reduceByKey(f : (Rep[V], Rep[V]) => Rep[V] ) = vector_reduceByKey[K, V](x, f)
    }
    

    //operations
    def vector_new[A:Manifest](file : Rep[String]): Rep[Vector[A]]
    def vector_map[A : Manifest, B : Manifest](vector : Rep[Vector[A]], f : Rep[A] => Rep[B]) : Rep[Vector[B]]
    def vector_filter[A : Manifest](vector : Rep[Vector[A]], f: Rep[A] => Rep[Boolean]) : Rep[Vector[A]]
    def vector_save[A : Manifest](vector : Rep[Vector[A]], path : Rep[String]) : Rep[Unit]
    def vector_reduceByKey[K: Manifest, V : Manifest](vector : Rep[Vector[(K,V)]], f : (Rep[V], Rep[V]) => Rep[V] ) : Rep[Vector[(K, V)]]
    //def vector_apply[A:Manifest](x: Rep[Vector[A]], n: Rep[Int])(implicit ctx: SourceContext) : Rep[A]
}

trait VectorOpsExp extends VectorOps with VariablesExp with BaseFatExp with DeliteCollectionOpsExp {
  this: SimpleVectorExp =>
//   case class DeliteCollectionApply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) extends Def[A]
   
    case class NewVector[A : Manifest](file : Exp[String]) extends Def[Vector[A]] {
      val mA = manifest[A]
    }
    
    case class VectorMap[A : Manifest, B : Manifest](in : Exp[Vector[A]], func : Exp[A] => Exp[B]) //, convert : Exp[Int] => Exp[A])
       extends DeliteOpMap[A, B, Vector[B]] {
      type CB = Vector[B]
//      def convert 
      lazy val size = Const(1); //throw new UnsupportedOperationException()
      override lazy val body: Def[CB] = copyBodyOrElse(DeliteCollectElem[B, CB](
      aV = this.aV,
      alloc = null,
      func = reifyEffects(this.func(dc_apply(in,v)))
    ))

//    override lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
//      func = reifyEffects(this.func(this.convert(v))),
//      sync = unit(List())
//    ))
//       override lazy val body: Def[CB] = copyBodyOrElse(DeliteCollectElem[B, CB](
//      //alloc = reifyEffects(this.alloc),
//       alloc = reifyEffects(Vector[B]("mapAlloc")),
//      func = reifyEffects(this.func(v))
//    ))

   // Members declared in ppl.delite.framework.ops.DeliteOpsExp.DeliteOpMap
	   def alloc = vector_new(unit("mapalloc"))
      //def copyBodyor
      val mA = manifest[A]
      val mB = manifest[B]
    }

	case class VectorFilter[A : Manifest](in : Exp[Vector[A]], cond : Exp[A] => Exp[Boolean]) extends DeliteOpFilter[A, A, Vector[A]] {
       type CB = Vector[A]
//      def convert 
      lazy val size = Const(1); //throw new UnsupportedOperationException()
      def func = e => e
      override lazy val body: Def[CB] = copyBodyOrElse(DeliteCollectElem[A, CB](
      aV = this.aV,
      alloc = null,
      func = reifyEffects(this.func(dc_apply(in,v))),
      cond = reifyEffects(this.cond(dc_apply(in,v)))::Nil
    ))

//    override lazy val body: Def[Unit] = copyBodyOrElse(DeliteForeachElem(
//      func = reifyEffects(this.func(this.convert(v))),
//      sync = unit(List())
//    ))
//       override lazy val body: Def[CB] = copyBodyOrElse(DeliteCollectElem[B, CB](
//      //alloc = reifyEffects(this.alloc),
//       alloc = reifyEffects(Vector[B]("mapAlloc")),
//      func = reifyEffects(this.func(v))
//    ))

   // Members declared in ppl.delite.framework.ops.DeliteOpsExp.DeliteOpMap
	   def alloc = vector_new(unit("mapalloc"))

      val mA = manifest[A]
    }

    case class VectorSave[A : Manifest](vector : Exp[Vector[A]], path : Rep[String]) extends Def[Unit] {
      val mA = manifest[A]
    }
    
    case class VectorReduceByKey[K: Manifest, V : Manifest](vector : Exp[Vector[(K,V)]], f : (Exp[V], Exp[V]) => Exp[V]) extends Def[Vector[(K, V)]]
    
    override def vector_new[A: Manifest](file : Exp[String]) = reflectMutable(NewVector[A](file))
    override def vector_map[A : Manifest, B : Manifest](vector : Exp[Vector[A]], f : Exp[A] => Exp[B]) = reflectPure(VectorMap[A, B](vector, f))
	override def vector_filter[A : Manifest](vector : Exp[Vector[A]], f: Exp[A] => Exp[Boolean]) = reflectPure(VectorFilter[A](vector, f))
    override def vector_save[A : Manifest](vector : Exp[Vector[A]], path : Exp[String]) = reflectEffect(VectorSave[A](vector, path)) //, manifest[A].erasure.asInstanceOf[Class[A]]))
    override def vector_reduceByKey[K: Manifest, V : Manifest](vector : Exp[Vector[(K,V)]], f : (Exp[V], Exp[V]) => Exp[V] ) = reflectPure(VectorReduceByKey(vector, f))
    override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext): Exp[A] = reflectPure(DeliteCollectionApply(x,n))
    //def dc_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = reflectPure(DeliteCollectionApply(x,n))
   //def dc_apply(i: Rep[Int])(implicit ctx: SourceContext) = vector_apply(x,i)
}

/**
 * Implementation using kernel embedding
 */
trait VectorImplOps { this: SimpleVector =>
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: SimpleVectorCompiler with SimpleVectorLift =>

}
