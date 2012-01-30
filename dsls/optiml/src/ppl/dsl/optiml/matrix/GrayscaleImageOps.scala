package ppl.dsl.optiml.matrix

import java.io.{PrintWriter}
import scala.virtualization.lms.common.{VariablesExp, Variables, CGenBase, CudaGenBase, ScalaGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config
import ppl.dsl.optiml.CudaGenDataStruct
import ppl.dsl.optiml.{Vector, Matrix, GrayscaleImage}
import ppl.dsl.optiml.{OptiML, OptiMLExp}

trait GrayscaleImageOps extends Variables {
  this: OptiML =>

  object GrayscaleImage {
    def apply(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = grayscaleimage_obj_new(numRows, numCols)
    def apply(x: Rep[Matrix[Int]])(implicit ctx: SourceContext) = grayscaleimage_obj_frommat(x)
    def cartToPolar(x: Rep[Matrix[Float]], y: Rep[Matrix[Float]])(implicit ctx: SourceContext) = grayscaleimage_obj_carttopolar(x,y)

//    val scharrYkernel = Matrix(Vector[Int](-3, -10, -3), Vector[Int](0, 0, 0), Vector[Int](3, 10, 3))
//    val scharrXkernel = scharrYkernel.t
  }

  implicit def repGrayscaleImageToGrayscaleImageOps[A:Manifest](x: Rep[GrayscaleImage]) = new grayscaleImageOpsCls(x)
  implicit def varToGrayscaleImageOps[A:Manifest](x: Var[GrayscaleImage]) = new grayscaleImageOpsCls(readVar(x))

  class grayscaleImageOpsCls(x: Rep[GrayscaleImage]) {
    import GrayscaleImage._

    def bitwiseOrDownsample()(implicit ctx: SourceContext) = GrayscaleImage(x.downsample(unit(2),unit(2)) { slice => slice(unit(0),unit(0)) | slice(unit(1),unit(0)) | slice(unit(0),unit(1)) | slice(unit(1),unit(1)) })
    def gradients(polar: Rep[Boolean] = unit(false))(implicit ctx: SourceContext) = { // unroll at call site for parallelism (temporary until we have composite op)
      val scharrYkernel = Matrix[Int](unit(3), unit(3))
      scharrYkernel(unit(0),unit(0)) = unit(-3); scharrYkernel(unit(0),unit(1)) = unit(-10); scharrYkernel(unit(0),unit(2)) = unit(-3)
      scharrYkernel(unit(2),unit(0)) = unit(3); scharrYkernel(unit(2),unit(1)) = unit(10); scharrYkernel(unit(2),unit(2)) = unit(3)
      val scharrXkernel = scharrYkernel.t
      val a = x.convolve(scharrXkernel)
      val b = x.convolve(scharrYkernel)
      if (polar) cartToPolar(a.toFloat,b.toFloat) else (a.toFloat,b.toFloat)
    }
    // TODO: need to refactor using CanBuildFrom and 2.8 techniques to avoid this duplication.
    //def convolve(kernel: Rep[Matrix[Int]]) = GrayscaleImage(x.windowedFilter(kernel.numRows, kernel.numCols) { slice => (slice *:* kernel).sum })
    def windowedFilter(rowDim: Rep[Int], colDim: Rep[Int])(block: Rep[Matrix[Int]] => Rep[Int])(implicit ctx: SourceContext) =
      GrayscaleImage(image_windowed_filter(x,rowDim, colDim, block))
  }

  // object defs
  def grayscaleimage_obj_new(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_obj_frommat(x: Rep[Matrix[Int]])(implicit ctx: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_obj_carttopolar(x: Rep[Matrix[Float]], y: Rep[Matrix[Float]])(implicit ctx: SourceContext): (Rep[Matrix[Float]],Rep[Matrix[Float]])
}


trait GrayscaleImageOpsExp extends GrayscaleImageOps with VariablesExp {
  this: OptiMLExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class GrayscaleImageObjectNew(numRows: Exp[Int], numCols: Exp[Int]) extends Def[GrayscaleImage]
  case class GrayscaleImageObjectFromMat(x: Exp[Matrix[Int]]) extends Def[GrayscaleImage]


  ////////////////////////////////
  // implemented via delite ops

  case class GrayscaleImageObjectCartToPolarMagnitude(inA: Exp[Matrix[Float]], inB: Exp[Matrix[Float]])
    extends MatrixArithmeticZipWith(inA, inB) {

    def func = (a,b) => sqrt(a*a + b*b).AsInstanceOf[Float]
  }

  case class GrayscaleImageObjectCartToPolarPhase(inA: Exp[Matrix[Float]], inB: Exp[Matrix[Float]])
    extends MatrixArithmeticZipWith(inA, inB) {

    def func = (a,b) => (atan2(b, a)*unit(180)/Pi).AsInstanceOf[Float]
  }

  ////////////////////
  // object interface

  def grayscaleimage_obj_new(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectEffect(GrayscaleImageObjectNew(numRows, numCols))
  def grayscaleimage_obj_frommat(x: Exp[Matrix[Int]])(implicit ctx: SourceContext) = reflectEffect(GrayscaleImageObjectFromMat(x))
  def grayscaleimage_obj_carttopolar(x: Exp[Matrix[Float]], y: Exp[Matrix[Float]])(implicit ctx: SourceContext) = {
    val mag = reflectPure(GrayscaleImageObjectCartToPolarMagnitude(x,y))
    val phase = reflectPure(GrayscaleImageObjectCartToPolarPhase(x,y)) map { a => if (a < unit(0f)) a + unit(360f) else a } 
    (mag,phase)
  }

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case GrayscaleImageObjectCartToPolarPhase(a,b) => reflectPure(new { override val original = Some(f,e) } with GrayscaleImageObjectCartToPolarPhase(f(a),f(b)))(mtype(manifest[A]), implicitly[SourceContext])
    case Reflect(GrayscaleImageObjectFromMat(x), u, es) => reflectMirrored(Reflect(GrayscaleImageObjectFromMat(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}


trait ScalaGenGrayscaleImageOps extends ScalaGenBase {
  val IR: GrayscaleImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case GrayscaleImageObjectNew(numRows, numCols) => emitValDef(sym, "new generated.scala.GrayscaleImageImpl(" + quote(numRows) + "," + quote(numCols) + ")")
    case GrayscaleImageObjectFromMat(m) => emitValDef(sym, "new generated.scala.GrayscaleImageImpl(" + quote(m) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenGrayscaleImageOps extends CudaGenBase with CudaGenDataStruct {
  val IR: GrayscaleImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenGrayscaleImageOps extends CGenBase {
  val IR: GrayscaleImageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
