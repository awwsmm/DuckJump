package duckjump

object Physics {

  class Vec[T](val x1: Double, val x2: Double) {
    def + (delta: Vec[T]): Vec[T] = new Vec[T](x1 + delta.x1, x2 + delta.x2)
    def - (delta: Vec[T]): Vec[T] = this + (-delta)

    def unary_- (): Vec[T] = new Vec[T](-x1, -x2)

    def * (factor: Vec[Nothing]): Vec[T] = new Vec[T](x1 * factor.x1, x2 * factor.x2)
    def * (factor: Double): Vec[T] = new Vec[T](x1 * factor, x2 * factor)

    override def toString: String = s"<$x1, $x2>"
  }

  object Vec {
    val Zero: Vec[Nothing] = new Vec[Nothing](0, 0)
    val Unit: Vec[Nothing] = new Vec[Nothing](1, 1)
    def apply[T](x1: Double, y1: Double) = new Vec[T](x1, y1)
  }

  case class Pos(x: Double, y: Double) extends Vec[Pos](x, y) {
    def + (vel: Vel): Pos = Pos(x + vel.x, y + vel.y)
    override def toString: String = s"Pos${super.toString}"
  }

  object Pos {
    val Zero: Pos = Pos(0, 0)
  }

  class Vel private(val x: Double, val y: Double) extends Vec[Vel](x, y) {
    def + (accel: Accel): Vel = Vel(x + accel.x, y + accel.y)
    override def toString: String = s"Vel${super.toString}"

    def copy(x: Double = this.x, y: Double = this.y): Vel = {
      new Vel(x, y)
    }
  }

  object Vel {
    val Zero: Vel = new Vel(0, 0)
    val Epsilon: Double = 0.01

    def apply(x: Double, y: Double): Vel = {
      val xe = if (Math.abs(x) < Epsilon) 0 else x
      val ye = if (Math.abs(y) < Epsilon) 0 else y
      new Vel(xe, ye)
    }
  }

  class Accel private(val x: Double, val y: Double) extends Vec[Accel](x, y) {
    override def toString: String = s"Accel${super.toString}"

    def copy(x: Double = this.x, y: Double = this.y): Accel = {
      new Accel(x, y)
    }
  }

  object Accel {
    val Zero: Accel = new Accel(0, 0)
    val Epsilon: Double = 0.001

    def apply(x: Double, y: Double): Accel = {
      val xe = if (Math.abs(x) < Epsilon) 0 else x
      val ye = if (Math.abs(y) < Epsilon) 0 else y
      new Accel(xe, ye)
    }
  }

  object Implicits {
    implicit def pos2Pos (pos: Vec[Pos]): Pos = Pos(pos.x1, pos.x2)
    implicit def vel2Vel (vel: Vec[Vel]): Vel = Vel(vel.x1, vel.x2)
    implicit def accel2Accel (accel: Vec[Accel]): Accel = Accel(accel.x1, accel.x2)

    implicit def vec2Pos (vec: Vec[Nothing]): Pos = Pos(vec.x1, vec.x2)
    implicit def vec2Vel (vec: Vec[Nothing]): Vel = Vel(vec.x1, vec.x2)
    implicit def vec2Accel (vec: Vec[Nothing]): Accel = Accel(vec.x1, vec.x2)
  }

  val Gravity: Accel = Accel(0, -5)

  // Vec(0   , 1) == Frictionless
  // Vec(0.01, 1) == Ice
  // Vec(0.10, 1) == default
  // Vec(0.25, 1) == max friction where max speed can be achieved
  // Vec(1   , 1) == can change direction, but no movement at all
  val Friction: Vec[Nothing] = Vec(0.1, 0)

}