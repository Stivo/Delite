package ppl.dsl.deliszt.datastruct.scala

object Vec3Impl {
  def apply[T:ClassManifest](v0: T, v1: T, v2: T) = {
    new Vec3Impl[T](v0, v1, v2)
  }
}

class Vec3Impl[@specialized T: ClassManifest](var v0: T, var v1: T, var v2: T) extends Vec[T] with Copyable {
  
  override def size = 3

  def apply(n : Int) = {
    /*unsafe.UnsafeAccessor.unsafe.getT(this, 16 + n*UNSAFE_SIZE)*/ v0
  }
  
  def update(n : Int, v : T) = {
    /*unsafe.UnsafeAccessor.unsafe.putT(this, 16 + n*UNSAFE_SIZE, v)*/
  }
  
  def cloneL = {
    new Vec3Impl[T](v0, v1, v2)
  }
  
  def copy() = cloneL
  
  override def toString() = {
    "Vec[" + size + "](" + v0 + "," + v1 + "," + v2 + ")"
  }
}
