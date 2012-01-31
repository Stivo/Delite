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
		def save(path : Rep[String]) = vector_save(vector, path)
		def length = dc_length(vector)
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

    def dc_length[A:Manifest](x: Rep[DeliteCollection[A]])(implicit ctx: SourceContext): Rep[Int]

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
      val size = copyTransformedOrElse(_.size)(in.length)
      //lazy val size = Const(1); //throw new UnsupportedOperationException()
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
      val mVB = manifest[Vector[B]]
    }

	case class VectorFilter[A : Manifest](in : Exp[Vector[A]], cond : Exp[A] => Exp[Boolean]) extends DeliteOpFilter[A, A, Vector[A]] {
       type CB = Vector[A]
//      def convert 
      val size = copyTransformedOrElse(_.size)(in.length)
     // lazy val size = Const(1); //throw new UnsupportedOperationException()
      def func = e => e
      override lazy val body: Def[CB] = copyBodyOrElse(DeliteCollectElem[A, CB](
      aV = this.aV,
      alloc = null,
      func = reifyEffects(this.func(dc_apply(in,v))),
      cond = reifyEffects(this.cond(dc_apply(in,v)))::Nil
    ))

   // Members declared in ppl.delite.framework.ops.DeliteOpsExp.DeliteOpMap
	   def alloc = vector_new(unit("mapalloc"))

      val mA = manifest[A]
      val mVA = manifest[Vector[A]]
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
    override def dc_length[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext): Exp[Int] = reflectPure(DeliteCollectionSize(x))
    
    override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    	if (f.isInstanceOf[SubstTransformer]) {
    	  printlog("Transformer has map "+(f.asInstanceOf[SubstTransformer].subst))
    	}
    	val out =
    (e match {
	    case map@VectorMap(_, _) => reflectPure(new { override val original = Some(f,e) } with VectorMap(f(map.in), f(map.func))(mtype(map.mA), mtype(map.mB)))(mtype(map.mVB), ctx)
	    case filter@VectorFilter(_, _) => reflectPure( new { override val original = Some(f,e) } with VectorFilter(f(filter.in), f(filter.cond))(mtype(manifest[A])))(mtype(filter.mVA), ctx)
	    case VectorSave(vector, path) => vector_save(f(vector), f(path))
	    case StringIsEmpty(string) => reflectPure(StringIsEmpty(f(string)))(mtype(manifest[A]), ctx)
	    case StringTrim(string) => reflectPure(StringTrim(f(string)))
	    case StringContains(string, sub) => reflectPure(StringContains(f(string), f(sub)))
	    case StringStartsWith(string, start) => reflectPure(StringStartsWith(f(string), f(start)))
	    case SimpleLoop(size, sym, body) => reflectPure(SimpleLoop(f(size), f(sym).asInstanceOf[Sym[Int]], mirrorLoopBody(body, f)))
	    //reflectMirrored(Reflect(DeliteCollectionApply(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
	    case Reflect(filter@VectorFilter(in, cond), u, es) => 
	      reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorFilter(f(filter.in), f(filter.cond))(mtype(manifest[A])), mapOver(f, u), f(es)))(mtype(filter.mVA))
	    case Reflect(VectorSave(vector, path), u, es) => {printlog("mirror reflect vectorsave"); reflectMirrored(Reflect(VectorSave(f(vector), f(path)), mapOver(f,u), f(es))) (mtype(manifest[A]))}
	    case _ => printlog("did not match "+e.toString); super.mirror(e, f)
    });
    	printlog(e)
    	printlog(" becomes ==> ")
    	printlog(out match {
    	  case Def(s) => s.toString
    	  case _ => "not a def"
    	})
    	out.asInstanceOf[Exp[A]]
    	}
    
    /*
     * Fusion State:
     * General:
     * - Creates output collections which are not used
     * - problem with reify: emitting now valDef for it, otherwise after fusion wrong value is used
     * Can fuse:
     * - map with map
     * - map with filter
     * can not fuse:
     * - filter with filter: Condition does not get put into second filter.becomes Vector[Vector[A]]
     * - filter with Map: Condition gets evaluated but for wrong output collection
     */
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
