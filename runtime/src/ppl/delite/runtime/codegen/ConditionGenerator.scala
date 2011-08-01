package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.ops.{DeliteOP, OP_Condition}

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 1:33 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class ConditionGenerator(condition: OP_Condition, location: Int) extends NestedGenerator(condition, location) {

  def makeExecutable() {
    val out = new StringBuilder //the output string
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    val hasOutput = condition.outputType != "Unit"
    val inputs = (condition.predicateGraph.inputOps ++ condition.thenGraph.inputOps ++ condition.elseGraph.inputOps)

    updateOP()
    //header
    writeHeader(location, out)
    writeMethodHeader(out)

    val available = new ArrayBuffer[DeliteOP]
    //output predicate
    if (condition.predicateValue == "") {
      available ++= inputs
      addKernelCalls(condition.predicateGraph.schedule(location), location, out)
    }

    //write if
    out.append("if (")
    if (condition.predicateValue == "") out.append(getSym(condition.predicateGraph.result._1, condition.predicateGraph.result._2))
    else out.append(condition.predicateValue)
    out.append(") {\n")

    //output if body
    if (condition.thenValue == "") {
      available.clear()
      available ++= inputs
      addKernelCalls(condition.thenGraph.schedule(location), location, out)
      if (hasOutput) out.append(getSym(condition.thenGraph.result._1, condition.thenGraph.result._2))
    }
    else if (hasOutput) out.append(condition.thenValue)
    if (hasOutput) out.append('\n')

    //print else
    out.append("} else {\n")

    //output else body
    if (condition.elseValue == "") {
      available.clear()
      available ++= inputs
      addKernelCalls(condition.elseGraph.schedule(location), location, out)
      if (hasOutput) out.append(getSym(condition.elseGraph.result._1, condition.elseGraph.result._2))
    }
    else if (hasOutput) out.append(condition.elseValue)
    if (hasOutput) out.append('\n')

    //print end of if and method
    out.append("}\n}\n")

    //the sync methods/objects
    addLocalSync(out)

    //the footer
    out.append("}\n")

    ScalaCompile.addSource(out.toString, kernelName)
  }

  override protected def getSync(op: DeliteOP, name: String) = {
    if (condition.predicateGraph.ops.contains(op))
      "Result_" + baseId + "P_" + name
    else if (condition.thenGraph.ops.contains(op))
      "Result_" + baseId + "T_" + name
    else
      "Result_" + baseId + "E_" + name
  }

  override protected def getSym(op: DeliteOP, name: String) = {
    if (condition.predicateGraph.ops.contains(op))
      "x" + baseId + "P_" + name
    else if (condition.thenGraph.ops.contains(op))
      "x" + baseId + "T_" + name
    else if (condition.elseGraph.ops.contains(op))
      "x" + baseId + "E_" + name
    else //input
      "x" + baseId + "_" + name
  }

  protected def executableName = "Condition_" + baseId + "_"

}

class GPUConditionGenerator(condition: OP_Condition, location: Int) extends GPUNestedGenerator(condition, location) {

  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    GPUMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaConditionGenerator(condition, location).emitScala(syncList), kernelName)
  }

  def emitCpp(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder //the output string
    val hasOutput = condition.outputType != "Unit"
    assert(hasOutput == false) //TODO: we can relax this by conditionally selecting the proper metadata functions as well
    val inputs = (condition.predicateGraph.inputOps ++ condition.thenGraph.inputOps ++ condition.elseGraph.inputOps)

    writeFunctionHeader(out)
    writeJNIInitializer(location, out)

    val available = new ArrayBuffer[DeliteOP]
    val awaited = new ArrayBuffer[DeliteOP]

    //output predicate
    if (condition.predicateValue == "") {
      available ++= inputs
      awaited ++= inputs
      addKernelCalls(condition.predicateGraph.schedule(location), location, available, awaited, syncList, out)
    }

    //write if
    out.append("if (")
    if (condition.predicateValue == "") out.append(getSymGPU(condition.predicateGraph.result._2))
    else out.append(condition.predicateValue)
    out.append(") {\n")

    //output if body
    if (condition.thenValue == "") {
      available.clear()
      available ++= inputs
      awaited.clear()
      awaited ++= inputs
      addKernelCalls(condition.thenGraph.schedule(location), location, available, awaited, syncList, out)
      if (hasOutput) {
        out.append("return ")
        out.append(getSymGPU(condition.thenGraph.result._2))
      }
    }
    else if (hasOutput) {
      out.append("return ")
      out.append(condition.thenValue)
    }
    if (hasOutput) out.append(";\n")

    //print else
    out.append("} else {\n")

    //output else body
    if (condition.elseValue == "") {
      available.clear()
      available ++= inputs
      awaited.clear()
      awaited ++= inputs
      addKernelCalls(condition.elseGraph.schedule(location), location, available, awaited, syncList, out)
      if (hasOutput) {
        out.append("return ")
        out.append(getSymGPU(condition.elseGraph.result._2))
      }
    }
    else if (hasOutput) {
      out.append("return ")
      out.append(condition.elseValue)
    }
    if (hasOutput) out.append(";\n")

    //print end of if and function
    out.append("}\n}\n")

    out.toString
  }

  override protected def getScalaSym(op: DeliteOP, name: String) = {
    if (condition.predicateGraph.ops.contains(op))
      "x" + baseId + "P_" + name
    else if (condition.thenGraph.ops.contains(op))
      "x" + baseId + "T_" + name
    else if (condition.elseGraph.ops.contains(op))
      "x" + baseId + "E_" + name
    else //input
      "x" + baseId + "_" + name
  }

  protected def executableName = "Condition_" + baseId + "_"

}

class GPUScalaConditionGenerator(condition: OP_Condition, location: Int) extends GPUScalaNestedGenerator(condition, location) {
  protected def executableName = "Condition_" + baseId + "_"
}
        