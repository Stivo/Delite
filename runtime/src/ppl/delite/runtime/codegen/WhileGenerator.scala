package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.ops.{DeliteOP, OP_While}

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 4:09 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class WhileGenerator(whileLoop: OP_While, location: Int) extends NestedGenerator(whileLoop, location) {

  def makeExecutable() {
    val out = new StringBuilder //the output string
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    val inputs = (whileLoop.predicateGraph.inputOps ++ whileLoop.bodyGraph.inputOps)

    updateOP()
    //header
    writeHeader(location, out)
    writeMethodHeader(out)

    val available = new ArrayBuffer[DeliteOP]
    //output predicate
    if (whileLoop.predicateValue == "") {
      out.append("def predicate: Boolean = {\n")
      available ++= inputs
      addKernelCalls(whileLoop.predicateGraph.schedule(location), location, out)
      out.append(getSym(whileLoop.predicateGraph.result._1, whileLoop.predicateGraph.result._2))
      out.append("\n}\n")
    }

    //write while
    out.append("while (")
    if (whileLoop.predicateValue == "")
      out.append("predicate")
    else
      out.append(whileLoop.predicateValue)
    out.append(") {\n")

    //output while body
    if (whileLoop.bodyValue == "") {
      available.clear()
      available ++= inputs
      addKernelCalls(whileLoop.bodyGraph.schedule(location), location, out)
    }

    //print end of while and method
    out.append("}\n}\n")

    //the sync methods/objects
    addLocalSync(out)

    //the footer
    out.append("}\n")

    ScalaCompile.addSource(out.toString, kernelName)
  }

  override protected def getSym(op: DeliteOP, name: String) = WhileCommon.getSym(whileLoop, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = WhileCommon.getSync(whileLoop, baseId, op, name)

  protected def executableName = "While_" + baseId + "_"

}

class GPUWhileGenerator(whileLoop: OP_While, location: Int) extends GPUNestedGenerator(whileLoop, location) {

  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    GPUMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaWhileGenerator(whileLoop, location).emitScala(syncList), kernelName)
  }

  def emitCpp(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder //the output string
    val inputs = (whileLoop.predicateGraph.inputOps ++ whileLoop.bodyGraph.inputOps)

    writeFunctionHeader(out)
    writeJNIInitializer(location, out)

    val available = new ArrayBuffer[DeliteOP]
    val awaited = new ArrayBuffer[DeliteOP]
    //output predicate
    if (whileLoop.predicateValue == "") {
      available ++= inputs
      awaited ++= inputs
      addKernelCalls(whileLoop.predicateGraph.schedule(location), location, available, awaited, syncList, out)
    }

    //write while
    if (whileLoop.predicateValue == "") {
      out.append("bool pred = ")
      out.append(getSymGPU(whileLoop.predicateGraph.result._2))
      out.append(";\n")
      out.append("while (pred")
    }
    else {
      out.append("while (")
      out.append(whileLoop.predicateValue)
    }
    out.append(") {\n")

    //output while body
    if (whileLoop.bodyValue == "") {
      available.clear()
      available ++= inputs
      awaited.clear()
      awaited ++= inputs
      addKernelCalls(whileLoop.bodyGraph.schedule(location), location, available, awaited, syncList, out)
    }

    //reevaluate predicate
    if (whileLoop.predicateValue == "") {
      available.clear()
      available ++= inputs
      awaited.clear()
      awaited ++= inputs
      out.append("{\n")
      addKernelCalls(whileLoop.predicateGraph.schedule(location), location, available, awaited, new ArrayBuffer[DeliteOP], out) //dummy syncList b/c already added
      out.append("pred = ") //update var
      out.append(getSymGPU(whileLoop.predicateGraph.result._2))
      out.append(";\n}\n")
    }

    //print end of while and function
    out.append("}\n}\n")
    out.toString
  }

  override protected def getScalaSym(op: DeliteOP, name: String) = WhileCommon.getSym(whileLoop, baseId, op, name)

  protected def executableName = "While_" + baseId + "_"

}

class GPUScalaWhileGenerator(whileLoop: OP_While, location: Int) extends GPUScalaNestedGenerator(whileLoop, location) {
  protected def executableName = "While_" + baseId + "_"
  override protected def getSym(op: DeliteOP, name: String) = WhileCommon.getSym(whileLoop, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = WhileCommon.getSync(whileLoop, baseId, op, name)
}

private[codegen] object WhileCommon {
  private def suffix(whileLoop: OP_While, baseId: String, op: DeliteOP, name: String) = {
    if (whileLoop.predicateGraph.ops.contains(op))
      "x" + baseId + "P_" + name
    else if (whileLoop.bodyGraph.ops.contains(op))
      "x" + baseId + "B_" + name
    else //input
      "x"  + baseId + "_" + name
  }

  def getSym(whileLoop: OP_While, baseId: String, op: DeliteOP, name: String) = "x" + suffix(whileLoop, baseId, op, name)
  def getSync(whileLoop: OP_While, baseId: String, op: DeliteOP, name: String) = "Result_" + suffix(whileLoop, baseId, op, name)
}
