package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import ppl.delite.framework.codegen.scala._
import java.io._

object BLAS extends ExternalLibrary {
  //val target = "scala" // this should be well-typed, but we don't have an IR reference yet, so we need to generalize that...  
  val libName = "scalaBLAS"
  val configFile = "BLAS.xml"  
  val ext = "c"
  // should we consider library linking machine dependent? do we have a different external lib
  // for unix and windows?
  val compileFlags = List( "-w", "-O3", "-lmkl_intel_lp64", "-lmkl_intel_thread", "-lmkl_core", "-liomp5", "-lmkl_mc3", "-lmkl_def", "-lgfortran",
                           "-shared", "-fPIC") // dynamic shared library
  val outputSwitch = "-o"
  
  override val header = """
#include <stdlib.h>
#include <stdio.h>

#include <jni.h>
#include "mkl.h"
"""
}
