package ppl.delite.runtime.graph.targets

import ppl.delite.runtime.graph.ops.DeliteOP

/**
 * Author: Kevin J. Brown
 * Date: Dec 4, 2010
 * Time: 10:48:52 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

final class CudaMetadata extends Serializable {

  val blockSizeX = new OPData
  val blockSizeY = new OPData
  val blockSizeZ = new OPData
  val dimSizeX = new OPData
  val dimSizeY = new OPData
  var inputs = Map[(DeliteOP, String), OPData]()
  var temps: List[(OPData, String)] = Nil
  var tempOps: List[DeliteOP] = Nil
  var outputs: List[(OPData, String)] = Nil

  def apply(field: String) = field match {
    case "gpuBlockSizeX" => blockSizeX
    case "gpuBlockSizeY" => blockSizeY
    case "gpuBlockSizeZ" => blockSizeZ
    case "gpuDimSizeX" => dimSizeX
    case "gpuDimSizeY" => dimSizeY
    case other => sys.error("unknown field: " + other)
  }

  def newInput(op: DeliteOP, sym: String) = {
    val in = new OPData
    inputs += (op,sym) -> in
    in
  }

  def newTemp(sym: String) = {
    val temp = new OPData
    temps ::= (temp, sym)
    temp
  }

  def newOutput(sym: String) = {
    val output = new OPData
    outputs ::= (output, sym)
    output
  }

  def replaceInput(old: DeliteOP, op: DeliteOP, sym: String) {
    if (inputs contains (old, sym)) {
      val value = inputs((old, sym))
      inputs -= Pair(old, sym)
      inputs += (op, sym) -> value

      blockSizeX.replaceInput(old, op, sym)
      blockSizeY.replaceInput(old, op, sym)
      blockSizeZ.replaceInput(old, op, sym)
      dimSizeX.replaceInput(old, op, sym)
      dimSizeY.replaceInput(old, op, sym)
      for ((temp,name) <- temps) temp.replaceInput(old, op, sym)
      for ((output,name) <- outputs) output.replaceInput(old, op, sym)
    }
  }

}

final class OPData extends Serializable {

  var func: String = _
  var funcReturn: String = _
  var inputs: List[(DeliteOP,String)] = Nil
  var resultType: String = _

  private[targets] def replaceInput(old: DeliteOP, op: DeliteOP, sym: String) {
    if (inputs contains (old, sym))
      inputs = inputs.patch(inputs.indexOf((old,sym)), List((op,sym)), 1)
  }

}
