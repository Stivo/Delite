package ppl.dsl.optiml

import java.io._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.c.TargetC
import ppl.delite.framework.codegen.delite.overrides.{DeliteCudaGenAllOverrides, DeliteCGenAllOverrides, DeliteScalaGenAllOverrides, DeliteAllOverridesExp}
import ppl.delite.framework.ops._
import ppl.dsl.optiml.datastruct.CudaGenDataStruct
import ppl.dsl.optiml.io._
import ppl.dsl.optiml.vector._
import ppl.dsl.optiml.matrix._
import ppl.dsl.optiml.graph._
import ppl.dsl.optiml.stream._
import ppl.dsl.optiml.capabilities._
import ppl.dsl.optiml.library.cluster._


/**
 * These separate OptiML applications from the Exp world.
 */

// ex. object GDARunner extends OptiMLApplicationRunner with GDA
trait OptiMLApplicationRunner extends OptiMLApplication with DeliteApplication with OptiMLExp

// ex. trait GDA extends OptiMLApplication
trait OptiMLApplication extends OptiML with OptiMLLift with OptiMLLibrary {
  var args: Rep[Array[String]]
  def main(): Unit
}

trait OptiMLLibrary extends OptiMLKmeans {
  this: OptiMLApplication =>
}

/**
 * These are the portions of Scala imported into OptiML's scope.
 */
trait OptiMLLift extends LiftVariables with LiftEquals with LiftString with LiftBoolean with LiftNumeric {
  this: OptiML =>
}

trait OptiMLScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with Functions
  with ImplicitOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with MathOps with CastingOps with ObjectOps with IOOps
  // only included because of args. TODO: investigate passing args as a vector
  with ArrayOps

trait OptiMLScalaOpsPkgExp extends OptiMLScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
  with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
  with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
  with SynchronizedArrayBufferOpsExp

trait OptiMLScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
  with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
  with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps with ScalaGenObjectOps
  with ScalaGenSynchronizedArrayBufferOps
  { val IR: OptiMLScalaOpsPkgExp  }

trait OptiMLCudaCodeGenPkg extends CudaGenDSLOps with CudaGenImplicitOps with CudaGenOrderingOps
  with CudaGenEqual with CudaGenIfThenElse with CudaGenVariables with CudaGenWhile with CudaGenFunctions
  with CudaGenStringOps with CudaGenRangeOps with CudaGenIOOps with CudaGenArrayOps with CudaGenBooleanOps
  with CudaGenPrimitiveOps with CudaGenMiscOps
  with CudaGenListOps with CudaGenSeqOps with CudaGenMathOps with CudaGenCastingOps with CudaGenSetOps
  with CudaGenSynchronizedArrayBufferOps
  { val IR: OptiMLScalaOpsPkgExp  }

trait OptiMLCCodeGenPkg extends CGenDSLOps with CGenImplicitOps with CGenOrderingOps
  with CGenStringOps with CGenRangeOps with CGenIOOps with CGenArrayOps with CGenBooleanOps
  with CGenPrimitiveOps with CGenMiscOps with CGenFunctions with CGenEqual with CGenIfThenElse
  with CGenVariables with CGenWhile with CGenListOps with CGenSeqOps
  with CGenSynchronizedArrayBufferOps
  { val IR: OptiMLScalaOpsPkgExp  }

/**
 * This the trait that every OptiML application must extend.
 */
trait OptiML extends OptiMLScalaOpsPkg with LanguageOps with ApplicationOps 
  with ArithOps with CloneableOps with HasMinMaxOps
  with VectorOps with MatrixOps with MLInputReaderOps with MLOutputWriterOps with VectorViewOps
  with IndexVectorOps with IndexVector2Ops with MatrixRowOps with MatrixColOps
  with StreamOps with StreamRowOps
  with GraphOps with VerticesOps with EdgeOps with VertexOps with MessageEdgeOps with MessageVertexOps with VSetOps
  with LabelsOps with TrainingSetOps with ImageOps with GrayscaleImageOps {

  this: OptiMLApplication =>
}

// these ops are only available to the compiler (they are restricted from application use)
trait OptiMLCompiler extends OptiML with RangeOps with IOOps with SeqOps with SetOps with ListOps {
  this: OptiMLApplication with OptiMLExp =>
}


/**
 * These are the corresponding IR nodes for OptiML.
 */
trait OptiMLExp extends OptiMLCompiler with OptiMLScalaOpsPkgExp with DeliteOpsExp with VariantsOpsExp 
  with LanguageOpsExp with ApplicationOpsExp
  with ArithOpsExpOpt 
  with VectorOpsExpOpt with MatrixOpsExpOpt with MLInputReaderOpsExp with MLOutputWriterOpsExp with VectorViewOpsExp
  with IndexVectorOpsExp with IndexVector2OpsExp with MatrixRowOpsExpOpt with MatrixColOpsExpOpt
  with StreamOpsExpOpt with StreamRowOpsExpOpt
  with LabelsOpsExp with TrainingSetOpsExp with ImageOpsExp with GrayscaleImageOpsExp
  with LanguageImplOpsStandard with VectorImplOpsStandard with VectorViewImplOpsStandard with IndexVectorImplOpsStandard
  with MatrixImplOpsStandard with MLInputReaderImplOpsStandard with MLOutputWriterImplOpsStandard with StreamImplOpsStandard
  with GraphOpsExp with VerticesOpsExp with EdgeOpsExp with VertexOpsExp with MessageEdgeOpsExp with MessageVertexOpsExp with VSetOpsExp
  with DeliteAllOverridesExp {

  // this: OptiMLApplicationRunner => why doesn't this work?
  this: DeliteApplication with OptiMLApplication with OptiMLExp => // can't be OptiMLApplication right now because code generators depend on stuff inside DeliteApplication (via IR)

  def getCodeGenPkg(t: Target{val IR: OptiMLExp.this.type}) : GenericFatCodegen{val IR: OptiMLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiMLCodeGenScala{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetCuda => new OptiMLCodeGenCuda{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _:TargetC => new OptiMLCodeGenC{val IR: OptiMLExp.this.type = OptiMLExp.this} 
      case _ => throw new RuntimeException("optiml does not support this target")
    }
  }

}


/**
 * OptiML code generators
 */
trait OptiMLCodeGenBase extends GenericFatCodegen {

  val IR: DeliteApplication with OptiMLExp
  override def initialDefs = IR.deliteGenerator.availableDefs


  def dsmap(line: String) = line

  val specialize = Set[String]()
  val specialize2 = Set[String]()
  def genSpec(f: File, outPath: String) = {}
  def genSpec2(f: File, outPath: String) = {}

  override def emitDataStructures(path: String) {
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"optiml"+s+"src"+s+"ppl"+s+"dsl"+s+"optiml"+s+"datastruct"+s + this.toString

    val dsDir = new File(dsRoot)
    if (!dsDir.exists) return
    val outDir = new File(path)
    outDir.mkdirs()

    for (f <- dsDir.listFiles) {
      if (specialize contains (f.getName.substring(0, f.getName.indexOf(".")))) {
        genSpec(f, path)
      }
      if (specialize2 contains (f.getName.substring(0, f.getName.indexOf(".")))) {
        genSpec2(f, path)
      }
      val outFile = path + s + f.getName
      val out = new BufferedWriter(new FileWriter(outFile))
      for (line <- scala.io.Source.fromFile(f).getLines) {
        out.write(dsmap(line) + "\n")
      }
      out.close()
    }
  }
}

trait OptiMLCodeGenScala extends OptiMLCodeGenBase with OptiMLScalaCodeGenPkg with ScalaGenDeliteOps with ScalaGenLanguageOps
  with ScalaGenApplicationOps
  with ScalaGenArithOps with ScalaGenVectorOps with ScalaGenVectorViewOps with ScalaGenMatrixOps
  with ScalaGenIndexVectorOps with ScalaGenIndexVector2Ops with ScalaGenMatrixRowOps with ScalaGenMatrixColOps
  with ScalaGenStreamOps with ScalaGenStreamRowOps
  with ScalaGenGraphOps with ScalaGenVerticesOps with ScalaGenEdgeOps with ScalaGenVertexOps with ScalaGenMessageEdgeOps with ScalaGenMessageVertexOps with ScalaGenVSetOps
  with ScalaGenLabelsOps with ScalaGenTrainingSetOps with ScalaGenVariantsOps with ScalaGenDeliteCollectionOps
  with ScalaGenImageOps with ScalaGenGrayscaleImageOps
  with DeliteScalaGenAllOverrides { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with OptiMLExp

  override val specialize = Set("VectorImpl", "MatrixImpl", "VectorViewImpl", "LabelsImpl",
                                "ImageImpl", "StreamImpl", "MatrixRowImpl", "MatrixColImpl", "StreamRowImpl")
  override val specialize2 = Set("TrainingSetImpl")

  override def genSpec(f: File, dsOut: String) {
    for (s <- List("Double","Int","Float","Long","Boolean")) {
      val outFile = dsOut + s + f.getName
      val out = new BufferedWriter(new FileWriter(outFile))
      for (line <- scala.io.Source.fromFile(f).getLines) {
        out.write(specmap(line, s) + "\n")
      }
      out.close()
    }
  }

  override def genSpec2(f: File, dsOut: String) {
    for (s1 <- List("Double","Int","Float","Long","Boolean")) {
      for (s2 <- List("Double","Int","Float","Long","Boolean")) {
        val outFile = dsOut + s1 + s2 + f.getName
        val out = new BufferedWriter(new FileWriter(outFile))
        for (line <- scala.io.Source.fromFile(f).getLines) {
          out.write(specmap2(line, s1, s2) + "\n")
        }
        out.close()
    }
    }
  }

  def specmap(line: String, t: String) : String = {
    var res = line.replaceAll("object ", "object " + t)
    res = res.replaceAll("import ", "import " + t)
    res = res.replaceAll("@specialized T: ClassManifest", t)
    res = res.replaceAll("T:Manifest", t)
    res = res.replaceAll("\\bT\\b", t)
    parmap(res)
  }

  def specmap2(line: String, t1: String, t2: String) : String = {
    var res = line.replaceAll("object ", "object " + t1 + t2)
    res = res.replaceAll("import ", "import " + t1 + t2)
    res = res.replaceAll("@specialized T: ClassManifest", t1)
    res = res.replaceAll("@specialized L: ClassManifest", t2)
    res = res.replaceAll("T:Manifest", t1)
    res = res.replaceAll("L:Manifest", t2)
    res = res.replaceAll("\\bT\\b", t1)
    res = res.replaceAll("\\bL\\b", t2)
    parmap(res)
  }

  override def remap[A](m: Manifest[A]): String = {
    parmap(super.remap(m))
  }

  def parmap(line: String): String = {
    var res = line
    for(tpe1 <- List("Int","Long","Double","Float","Boolean")) {
      for (s <- specialize) {
        res = res.replaceAll(s+"\\["+tpe1+"\\]", tpe1+s)
      }
      for(tpe2 <- List("Int","Long","Double","Float","Boolean")) {
        for (s <- specialize2) {
          // should probably parse and trim whitespace, this is fragile
          res = res.replaceAll(s+"\\["+tpe1+","+tpe2+"\\]", tpe1+tpe2+s)
          res = res.replaceAll(s+"\\["+tpe1+", "+tpe2+"\\]", tpe1+tpe2+s)
        }
      }
    }
    dsmap(res)
  }

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.optiml.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res
  }
}

trait OptiMLCodeGenCuda extends OptiMLCodeGenBase with OptiMLCudaCodeGenPkg /*with CudaGenLanguageOps*/ with CudaGenArithOps with CudaGenDeliteOps with CudaGenVectorOps with CudaGenMatrixOps with CudaGenDataStruct with CudaGenTrainingSetOps with CudaGenMatrixRowOps // with CudaGenVectorViewOps
  with CudaGenVariantsOps with DeliteCudaGenAllOverrides // with DeliteCodeGenOverrideCuda // with CudaGenMLInputReaderOps  //TODO:DeliteCodeGenOverrideScala needed?
{
  val IR: DeliteApplication with OptiMLExp
  import IR._


  // Maps the scala type to cuda type
  override def remap[A](m: Manifest[A]) : String = {
    m.toString match {
      case "ppl.dsl.optiml.datastruct.scala.Matrix[Int]" => "Matrix<int>"
      case "ppl.dsl.optiml.datastruct.scala.Matrix[Long]" => "Matrix<long>"
      case "ppl.dsl.optiml.datastruct.scala.Matrix[Float]" => "Matrix<float>"
      case "ppl.dsl.optiml.datastruct.scala.Matrix[Double]" => "Matrix<double>"
      case "ppl.dsl.optiml.datastruct.scala.Matrix[Boolean]" => "Matrix<bool>"
      case "ppl.dsl.optiml.datastruct.scala.Vector[Int]" => "Vector<int>"
      case "ppl.dsl.optiml.datastruct.scala.Vector[Long]" => "Vector<long>"
      case "ppl.dsl.optiml.datastruct.scala.Vector[Float]" => "Vector<float>"
      case "ppl.dsl.optiml.datastruct.scala.Vector[Double]" => "Vector<double>"
      case "ppl.dsl.optiml.datastruct.scala.Vector[Boolean]" => "Vector<bool>"
      case "ppl.dsl.optiml.datastruct.scala.RangeVector" => "RangeVector"
      case "ppl.dsl.optiml.datastruct.scala.IndexVector" => "IndexVector"
      case "ppl.dsl.optiml.datastruct.scala.Labels[Int]" => "Labels<int>"
      case "ppl.dsl.optiml.datastruct.scala.Labels[Long]" => "Labels<long>"
      case "ppl.dsl.optiml.datastruct.scala.Labels[Float]" => "Labels<float>"
      case "ppl.dsl.optiml.datastruct.scala.Labels[Double]" => "Labels<double>"
      case "ppl.dsl.optiml.datastruct.scala.Labels[Boolean]" => "Labels<bool>"
      case "ppl.dsl.optiml.datastruct.scala.TrainingSet[Double, Double]" => "TrainingSet<double,double>"
      case _ => super.remap(m)
    }
  }

  override def isObjectType[T](m: Manifest[T]) : Boolean = m.toString match {
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Int]" => true
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Long]" => true
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Float]" => true
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Double]" => true
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Boolean]" => true
    case "ppl.dsl.optiml.datastruct.scala.Vector[Int]" => true
    case "ppl.dsl.optiml.datastruct.scala.Vector[Long]" => true
    case "ppl.dsl.optiml.datastruct.scala.Vector[Float]" => true
    case "ppl.dsl.optiml.datastruct.scala.Vector[Double]" => true
    case "ppl.dsl.optiml.datastruct.scala.Vector[Boolean]" => true
    case "ppl.dsl.optiml.datastruct.scala.RangeVector" => true
    case "ppl.dsl.optiml.datastruct.scala.IndexVector" => true
    case "ppl.dsl.optiml.datastruct.scala.Labels[Int]" => true
    case "ppl.dsl.optiml.datastruct.scala.Labels[Long]" => true
    case "ppl.dsl.optiml.datastruct.scala.Labels[Float]" => true
    case "ppl.dsl.optiml.datastruct.scala.Labels[Double]" => true
    case "ppl.dsl.optiml.datastruct.scala.Labels[Boolean]" => true
    case "ppl.dsl.optiml.datastruct.scala.TrainingSet[Double, Double]" => true
    case _ => super.isObjectType(m)
  }

  override def copyInputHtoD(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Matrix<int>" | "Matrix<long>" | "Matrix<float>" | "Matrix<double>" | "Matrix<bool>" => matrixCopyInputHtoD(sym)
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => vectorCopyInputHtoD(sym)
    case "Labels<int>" | "Labels<long>" | "Labels<float>" | "Labels<double>" | "Labels<bool>" => labelsCopyInputHtoD(sym)
    case "RangeVector" => rangeVectorCopyInputHtoD(sym)
    case "IndexVector" => indexVectorCopyInputHtoD(sym)
    case "TrainingSet<double,double>" => trainingSetCopyInputHtoD(sym)
    case _ => super.copyInputHtoD(sym)
  }

  override def copyOutputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Matrix<int>" | "Matrix<long>" | "Matrix<float>" | "Matrix<double>" | "Matrix<bool>" => matrixCopyOutputDtoH(sym)
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => vectorCopyOutputDtoH(sym)
    case _ => super.copyOutputDtoH(sym)
  }

  override def copyMutableInputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Matrix<int>" | "Matrix<long>" | "Matrix<float>" | "Matrix<double>" | "Matrix<bool>" => matrixCopyMutableInputDtoH(sym)
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => vectorCopyMutableInputDtoH(sym)
    case "Labels<int>" | "Labels<long>" | "Labels<float>" | "Labels<double>" | "Labels<bool>" => labelsCopyMutableInputDtoH(sym)
    case "RangeVector" => rangeVectorCopyMutableInputDtoH(sym)
    case "IndexVector" => indexVectorCopyMutableInputDtoH(sym)
    case "TrainingSet<double,double>" => trainingSetCopyMutableInputDtoH(sym)
    case _ => super.copyMutableInputDtoH(sym)
  }

  /*
  override def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit = remap(newSym.Type) match {
    case "Matrix<int>" | "Matrix<long>" | "Matrix<float>" | "Matrix<double>" | "Matrix<bool>" => emitMatrixAllocSym(newSym,sym,reset)
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => emitVectorAllocSym(newSym,sym,reset)
    case _ => super.allocOutput(newSym,sym,reset)
  }
  */

  /*
  override def allocReference(newSym: Sym[Any], sym: Sym[Any]) : Unit = remap(newSym.Type) match {
    case "Matrix<int>" | "Matrix<long>" | "Matrix<float>" | "Matrix<double>" | "Matrix<bool>" => emitMatrixAllocRef(newSym,sym)
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => emitVectorAllocRef(newSym,sym)
    case "Labels<int>" | "Labels<long>" | "Labels<float>" | "Labels<double>" | "Labels<bool>" => emitVectorAllocRef(newSym,sym)
    case _ => super.allocReference(newSym,sym)
  }
   */

  override def positionMultDimInputs(sym: Sym[Any]) : String = remap(sym.Type) match {
    //TODO: Add matrix reposition, and also do safety check for datastructures that do not have data field
    case "Vector<int>" | "Vector<long>" | "Vector<float>" | "Vector<double>" | "Vector<bool>" => vectorPositionMultDimInputs(sym)
    case _ => super.positionMultDimInputs(sym)
  }

  override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.append("#include <float.h>\n")
    out.append("#include \"VectorImpl.h\"\n")
    out.append("#include \"MatrixImpl.h\"\n")
    out.append("#include \"RangeVectorImpl.h\"\n")
    out.append("#include \"IndexVectorImpl.h\"\n")
    out.append("#include \"LabelsImpl.h\"\n")
    out.append("#include \"TrainingSetImpl.h\"\n")
    out.toString
  }

}

trait OptiMLCodeGenC extends OptiMLCodeGenBase with OptiMLCCodeGenPkg with CGenArithOps with CGenDeliteOps with CGenVectorOps with CGenMatrixOps with CGenMatrixRowOps
  with CGenVariantsOps with DeliteCGenAllOverrides
{
  val IR: DeliteApplication with OptiMLExp
  import IR._

  override def remap[A](m: Manifest[A]) : String = m.toString match {
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Int]" => "Matrix<int>"
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Long]" => "Matrix<long>"
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Float]" => "Matrix<float>"
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Double]" => "Matrix<double>"
    case "ppl.dsl.optiml.datastruct.scala.Matrix[Boolean]" => "Matrix<bool>"
    case "ppl.dsl.optiml.datastruct.scala.Vector[Int]" => "Vector<int>"
    case "ppl.dsl.optiml.datastruct.scala.Vector[Long]" => "Vector<long>"
    case "ppl.dsl.optiml.datastruct.scala.Vector[Float]" => "Vector<float>"
    case "ppl.dsl.optiml.datastruct.scala.Vector[Double]" => "Vector<double>"
    case "ppl.dsl.optiml.datastruct.scala.Vector[Boolean]" => "Vector<bool>"
    case _ => super.remap(m)
  }
}
