package ppl.delite.runtime.graph.targets

/**
 * Author: Kevin J. Brown
 * Date: Dec 4, 2010
 * Time: 4:16:29 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Targets extends Enumeration {
  val Scala = Value("scala")
  val Cuda = Value("cuda")
  val C = Value("c")

  /**
   * Return the value of a target
   */
  def target(s: String): Value = s.toLowerCase() match {
    case "scala" => Scala
    case "cuda" => Cuda
    case "c" => C
    case _ => throw new IllegalArgumentException("unsupported target: " + s)
  }

  /**
   * Create a Unit-type Map for the set of targets included in the input Map
   */
  def unitTypes(id: String, targets: Map[Value,Map[String,String]]): Map[Value,Map[String,String]] = {
    var unitMap = Map[Value,Map[String,String]]()
    for (target <- targets.keys) {
      unitMap += target -> Map(id -> unitType(target), "functionReturn" -> unitType(target))
    }
    unitMap
  }

  /**
   * Creates a Unit-type Map for all targets
   */
  def unitTypes(id: String): Map[Value,Map[String,String]] = {
    var unitMap = Map[Value,Map[String,String]]()
    for (target <- values) {
      unitMap += target -> Map(id -> unitType(target), "functionReturn" -> unitType(target))
    }
    unitMap
  }

  /**
   *  Returns the Unit-type for the specified target as a String
   */
  def unitType(target: Value): String = {
    target match {
      case Scala => "Unit"
      case Cuda => "void"
      case C => "void"
    }
  }

  /*
  def intType(target: Value): String = {
    target match {
      case Scala => "Int"
      case Cuda => "int"
      case JNI => "jint"
      case Cpp => "int"
    }
  }
  */
}

object HardwareTargets extends Enumeration {
  val cpu = Value("cpu")
  val gpu = Value("gpu")
}
