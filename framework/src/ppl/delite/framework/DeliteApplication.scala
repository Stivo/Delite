package ppl.delite.framework

import codegen.c.TargetC
import codegen.cuda.TargetCuda
import codegen.delite.{DeliteCodeGenPkg, DeliteCodegen, TargetDelite}
import codegen.scala.TargetScala
import codegen.Target
import externlib.ExternLibrary
import ops.DeliteOpsExp
import scala.tools.nsc.io._
import scala.virtualization.lms.common.{BaseExp, Base}
import java.io.{FileWriter, File, PrintWriter}
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}

trait DeliteApplication extends DeliteOpsExp with ScalaCompile {
  type DeliteApplicationTarget = Target{val IR: DeliteApplication.this.type}

  def getCodeGenPkg(t: DeliteApplicationTarget) : GenericFatCodegen{val IR: DeliteApplication.this.type}

  lazy val scalaTarget = new TargetScala{val IR: DeliteApplication.this.type = DeliteApplication.this}
  lazy val cudaTarget = new TargetCuda{val IR: DeliteApplication.this.type = DeliteApplication.this}
  lazy val cTarget = new TargetC{val IR: DeliteApplication.this.type = DeliteApplication.this}

  // TODO: this should be handled via command line options
  lazy val targets = List[DeliteApplicationTarget](scalaTarget , cudaTarget, cTarget)
  val generators: List[GenericFatCodegen{ val IR: DeliteApplication.this.type }] = targets.map(getCodeGenPkg(_))

  // TODO: refactor, this is from ScalaCompile trait
  lazy val codegen: ScalaCodegen { val IR: DeliteApplication.this.type } = 
    getCodeGenPkg(scalaTarget).asInstanceOf[ScalaCodegen { val IR: DeliteApplication.this.type }]

  // generators created by getCodeGenPkg will use the 'current' scope of the deliteGenerator as global scope
  val deliteGenerator = new DeliteCodeGenPkg { val IR : DeliteApplication.this.type = DeliteApplication.this;
                                               val generators = DeliteApplication.this.generators }

  var args: Rep[Array[String]] = _
  
  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")

    println("******Generating the program******")

    //clean up the code gen directory
    Directory(Path(Config.buildDir)).deleteRecursively()

    val stream =
      if (Config.degFilename == ""){
        new PrintWriter(System.out)
      }
      else {
        new PrintWriter(new FileWriter(Config.degFilename))
      }

    def writeModules(baseDir: String) {
      Directory(Path(baseDir)).createDirectory()
      val writer = new FileWriter(baseDir + "modules.dm")
      writer.write("datastructures:\n")
      writer.write("kernels:datastructures\n")
      writer.close()
    }

    for (g <- generators) {
      val baseDir = Config.buildDir + File.separator + g.toString + File.separator
      writeModules(baseDir)
      g.emitDataStructures(baseDir + "datastructures" + File.separator)
      g.generatorInit(baseDir + "kernels" + File.separator)
    }

    //Emit and Compile external library (MKL BLAS)
    ExternLibrary.init()
    
    if (Config.degFilename.endsWith(".deg")) {
      val streamScala = new PrintWriter(new FileWriter(Config.degFilename.replace(".deg",".scala")))
      codegen.emitSource(liftedMain, "Application", streamScala) // whole scala application (for testing)
      // TODO: dot output
      reset
    }
    deliteGenerator.emitSource(liftedMain, "Application", stream)
  }

  final def generateScalaSource(name: String, stream: PrintWriter) = {
    codegen.emitSource(liftedMain, name, stream)
  }


  final def execute(args: Array[String]) {
    println("Delite Application Being Executed:[" + this.getClass.getSimpleName + "]")

    println("******Executing the program*********")
    globalDefs = List()
    val g = compile(liftedMain)
    g(args)
  }

  def registerDSLType(name: String): DSLTypeRepresentation = nop

  /**
   * this is the entry method for our applications, user implement this method. Note, that it is missing the
   * args parameter, args are now accessed via the args field. This basically hides the notion of Reps from
   * user code
   */
  def main(): Unit

  def liftedMain(x: Rep[Array[String]]) = { this.args = x; val y = main(); this.args = null; unit(y) }
  

  private def nop = throw new RuntimeException("not implemented yet")
}
