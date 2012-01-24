package epfl.dsl.distributed
//
import java.io.{BufferedWriter, FileWriter}
import scala.virtualization.lms.common._
import _root_.ppl.delite.framework.{Config, DeliteApplication}
import scala.tools.nsc.io._
import _root_.ppl.delite.framework.codegen.Target
import _root_.ppl.delite.framework.codegen.scala.TargetScala
import _root_.ppl.delite.framework.ops._
import _root_.ppl.delite.framework.datastructures._
import scala.virtualization.lms.internal.GenericFatCodegen
import java.io.PrintWriter
//import scala.virtualization.lms.ppl.ScalaGenIO

//
///**
// * Packages
// */
//
//the portions of Scala I want to include in my SimpleVector DSL:
//Reps version
trait SimpleVectorScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with Functions
  with ImplicitOps with NumericOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with MathOps with CastingOps with ObjectOps with ArrayOps

//Exps version
trait SimpleVectorScalaOpsPkgExp extends SimpleVectorScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with ImplicitOpsExp with NumericOpsExp with OrderingOpsExp with StringOpsExp
  with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
  with MathOpsExp with CastingOpsExp with ObjectOpsExp with ArrayOpsExp with RangeOpsExp
  with DeliteOpsExp with StructExp
//  
//Scala codegen version
trait SimpleVectorScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
  with ScalaGenImplicitOps with ScalaGenNumericOps with ScalaGenOrderingOps with ScalaGenStringOps
  with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
  with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenObjectOps with ScalaGenArrayOps with ScalaGenRangeOps
  with ScalaGenStruct
  { val IR: SimpleVectorScalaOpsPkgExp }
//

//
///**
// * add SimpleVector functionality
// */
//
trait SimpleVector extends SimpleVectorScalaOpsPkg with VectorOps { this: SimpleVectorApplication => }
//
////additional functionality I want available in the compiler, but not in the applications
trait SimpleVectorCompiler extends SimpleVector with RangeOps {
  this: SimpleVectorApplication with SimpleVectorExp =>
}
//
trait SimpleVectorExp extends SimpleVectorCompiler with SimpleVectorScalaOpsPkgExp with VectorOpsExp with VectorImplOpsStandard with DeliteOpsExp {
  this: DeliteApplication with SimpleVectorApplication =>

  def getCodeGenPkg(t: Target{val IR: SimpleVectorExp.this.type}) : GenericFatCodegen{val IR: SimpleVectorExp.this.type} = {
    t match {
      case _:TargetScala => new SimpleVectorCodegenScala{val IR: SimpleVectorExp.this.type = SimpleVectorExp.this}
      case _ => throw new RuntimeException("simple vector does not support this target")
    }
  }
}

trait SimpleVectorLift extends LiftVariables with LiftEquals with LiftString with LiftNumeric with LiftBoolean {
  this: SimpleVector =>
}
//
////the trait all SimpleVector applications must extend
trait SimpleVectorApplication extends SimpleVector with SimpleVectorLift {
  var args: Rep[Array[String]]
  def main()
}
//
////the runner for SimpleVector applications
trait SimpleVectorApplicationRunner extends SimpleVectorApplication with DeliteApplication with SimpleVectorExp
//
//
trait SimpleVectorCodegenBase extends GenericFatCodegen {
  val IR: DeliteApplication with SimpleVectorExp
  override def initialDefs = IR.deliteGenerator.availableDefs
  
  def dsmap(line: String) = line

  override def emitDataStructures(path: String) {
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"assignment2"+s+"src"+s+"ppl"+s+"dsl"+s+"assignment2"+s+"datastructures"+s + this.toString

    val dsDir = Directory(Path(dsRoot))
    val outDir = Directory(Path(path))
    outDir.createDirectory()

    for (f <- dsDir.files) {
      val outFile = path + s + f.name
      val out = new BufferedWriter(new FileWriter(outFile))
      for (line <- scala.io.Source.fromFile(f.jfile).getLines) {
        out.write(dsmap(line) + "\n")
      }
      out.close()
    }
  }
}
//
trait SimpleVectorCodegenScala extends SimpleVectorCodegenBase with SimpleVectorScalaCodeGenPkg
  with ScalaGenDeliteOps with ScalaGenVariantsOps {

  val IR: DeliteApplication with SimpleVectorExp
  import IR._
  
  //these methods translates types in the compiler to types in the generated code
  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.assignment2.datastructures", "generated")
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case nv@NewVector(file) => stream.println("Vector created from file "+file+ " of type "+nv.mA.toString())
    case vs@VectorSave(vector, file) => stream.println("Vector saved to file "+file+" of type "+vs.mA)
    case _ => super.emitNode(sym, rhs)
  }
////
////  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
////    case "Vector" => "Map[String,Any]"
////    case _ => super.remap(m)
////  }
}