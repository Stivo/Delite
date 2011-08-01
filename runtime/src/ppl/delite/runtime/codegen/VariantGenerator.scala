package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.ops.{DeliteOP, OP_Variant}

/**
 * Author: Kevin J. Brown
 * Date: 1/21/11
 * Time: 3:55 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class VariantGenerator(variant: OP_Variant, location: Int) extends NestedGenerator(variant, location) {

  def makeExecutable() {
    val out = new StringBuilder //the output string
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    val hasOutput = variant.outputType != "Unit"
    val inputs = variant.variantGraph.inputOps

    updateOP()
    //header
    writeHeader(location, out)
    writeMethodHeader(out)

    val available = new ArrayBuffer[DeliteOP]
    available ++= inputs

    //output body
    addKernelCalls(variant.variantGraph.schedule(location), location, out)
    if (hasOutput) {
      out.append(getSym(variant.variantGraph.result._1, variant.variantGraph.result._2))
      out.append('\n')
    }
    out.append("}\n") //end of method

    //the sync methods/objects
    addLocalSync(out)

    //the footer
    out.append("}\n")

    ScalaCompile.addSource(out.toString, kernelName)
  }

  protected def executableName = "Variant_" + baseId + "_"

}

class GPUVariantGenerator(variant: OP_Variant, location: Int) extends GPUNestedGenerator(variant, location) {

  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    GPUMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaVariantGenerator(variant, location).emitScala(syncList), kernelName)
  }

  def emitCpp(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder //the output string
    val hasOutput = variant.outputType != "Unit"
    val inputs = variant.variantGraph.inputOps


    writeFunctionHeader(out)
    writeJNIInitializer(location, out)

    val available = new ArrayBuffer[DeliteOP]
    val awaited = new ArrayBuffer[DeliteOP]
    available ++= inputs
    awaited ++= inputs

    //output body
    addKernelCalls(variant.variantGraph.schedule(location), location, available, awaited, syncList, out)
    if (hasOutput) {
      out.append("return ")
      out.append(getSymGPU(variant.variantGraph.result._2))
      out.append(";\n")
    }
    out.append("}\n") //end of function

    out.toString
  }

  protected def executableName = "Variant_" + baseId + "_"

}

class GPUScalaVariantGenerator(variant: OP_Variant, location: Int) extends GPUScalaNestedGenerator(variant, location) {
  protected def executableName = "Variant_" + baseId + "_"
}
