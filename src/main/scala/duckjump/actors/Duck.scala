package duckjump.actors

import akka.actor.{Actor, ActorLogging, ActorSystem}
import duckjump.Physics.{Accel, Pos, Vec, Vel}
import duckjump.Physics.Implicits._
import duckjump.{Main, Physics}
import org.scalajs.dom.document
import org.scalajs.dom.html.{Div, Image}
import scalatags.JsDom.all._

import scala.concurrent.ExecutionContextExecutor

case class Duck (init: Duck.State)(implicit system: ActorSystem, executor: ExecutionContextExecutor) extends Actor with ActorLogging {
  import Duck._

  private[this] var state = init

  def jumping (runningLanding: Boolean): Receive = {
    case Command.ReRender =>

      val (vx, xi) = (state.vel.x, state.pos.x)
      val (vy, yi) = (state.vel.y, state.pos.y)

      val xf = xi + vx
      val yf = yi + vy

      if (yf < Main.ground) {
        if (Duck.Debug) println(s"Duck will stop Jumping ($yf < ${Main.ground})")
        val newPos = Pos(xf, Main.ground)
        val newVel = Vel(vx, 0)
        val newAccel = Accel.Zero
        reRender(state.copy(pos = newPos, vel = newVel, accel = newAccel, sprite = sprite()))

        if (vx == 0 || !runningLanding) {
          context become receive
        } else {
          if (vx > 0) context become goingRight
          else context become goingLeft
        }

      } else {
        if (Duck.Debug) println("Duck is jumping")
        val newAccel = (state.accel * Vec(0, 1)) + Physics.Gravity
        reRender(state.copy(accel = newAccel, sprite = Duck.Sprite.Jumping))
      }

    case Command.Jump =>
      if (Duck.Debug) println("Double-Jump??")

    case Command.StopLeft =>
      if (state.vel.x < 0) {
        if (Duck.Debug) println("Duck will not run left upon landing")
        context become jumping(false)
      }

    case Command.StopRight =>
      if (state.vel.x > 0) {
        if (Duck.Debug) println("Duck will not run right upon landing")
        context become jumping(false)
      }

    case Command.GoLeft =>
      if (state.vel.x < 0) {
        if (Duck.Debug) println("Duck will run left upon landing")
        context become jumping(true)
      }

    case Command.GoRight =>
      if (state.vel.x > 0) {
        if (Duck.Debug) println("Duck will run right upon landing")
        context become jumping(true)
      }

    case e =>
      throw new Exception(s"UNHANDLED COMMAND $e RECEIVED in 'goingRight' STATE")
  }

  def goingRight: Receive = {
    case Command.ReRender | Command.GoRight =>
      goRight("Duck is going right")

    case Command.Jump =>
      jump("Duck will jump")
      context become jumping(true)

    case Command.GoLeft =>
      goLeft("Duck will start moving left")
      context become goingLeft

    case Command.StopRight =>
      if (Duck.Debug) println("Duck will stop moving right")
      context.unbecome()

    case Command.StopLeft =>

    case e =>
      throw new Exception(s"UNHANDLED COMMAND $e RECEIVED in 'goingRight' STATE")
  }

  def goingLeft: Receive = {
    case Command.ReRender | Command.GoLeft =>
      goLeft("Duck is going left")

    case Command.Jump =>
      jump("Duck will jump")
      context become jumping(true)

    case Command.GoRight =>
      goRight("Duck will start moving right")
      context become goingRight

    case Command.StopLeft =>
      if (Duck.Debug) println("Duck will stop moving left")
      context.unbecome()

    case Command.StopRight =>

    case e =>
      throw new Exception(s"UNHANDLED COMMAND $e RECEIVED in 'goingLeft' STATE")
  }

  private def withLogMsg (msg: String)(f: => Unit): Unit = {
    if (Duck.Debug && "" != msg) println(msg)
    f
  }

  private def goRight (msg: String = ""): Unit = withLogMsg(msg) {
    reRender(state.copy(accel = state.accel.copy(x = state.config.accel.x), sprite = sprite(), facingRight = true))
  }

  private def goLeft (msg: String = ""): Unit = withLogMsg(msg) {
    reRender(state.copy(accel = state.accel.copy(x = -state.config.accel.x), sprite = sprite(), facingRight = false))
  }

  private def jump (msg: String = ""): Unit = withLogMsg(msg) {
    reRender(state.copy(accel = state.accel.copy(y = state.config.accel.y), sprite = Duck.Sprite.Jumping))
  }

  override def receive: Receive = {

    case Command.GoRight =>
      goRight("Duck will start moving right")
      context become goingRight

    case Command.GoLeft =>
      goLeft("Duck will start moving left")
      context become goingLeft

    case Command.Jump =>
      jump("Duck will jump")
      context become jumping(false)

    case Command.ReRender =>
      if (Duck.Debug) println("Duck is re-rendering")
      val newVel = state.vel * (Vec.Unit - Physics.Friction)
      val newAccel = state.accel * Vec(0, 1)
      reRender(state.copy(vel = newVel, accel = newAccel, sprite = sprite()))

    case Command.StopLeft | Command.StopRight =>

    case e =>
      throw new Exception(s"UNHANDLED COMMAND $e RECEIVED in 'receive' STATE")
  }

  private def sprite(): Sprite = {
    if (Math.abs(state.vel.x) < 1) {
      Duck.Sprite.Idle

    } else if (Math.abs(state.vel.x) >= 0.75 * state.config.maxSpeed) {
      state.sprite match {
        case Sprite.Running => Sprite.Running2
        case _ => Sprite.Running
      }

    } else {
      state.sprite match {
        case Sprite.Walking => Sprite.Walking2
        case _ => Sprite.Walking
      }
    }
  }

  override def preStart(): Unit = {
    super.preStart()
    document.body.appendChild(state.rendered)
  }

  private def reRender (newState: Duck.State): Unit = {
    val newPos = newState.pos + newState.vel
    val newVel = newState.vel + newState.accel
    val newAccel = newState.accel

    val updatedState = newState.copy(pos = newPos, vel = newVel, accel = newAccel)

    if (Duck.Debug) {
      println(s"(x, vx, ax), (fx): (${updatedState.pos.x}, ${updatedState.vel.x}, ${updatedState.accel.x})")
      println(s"(y, vy, ay), (gy): (${updatedState.pos.y}, ${updatedState.vel.y}, ${updatedState.accel.y})")
    }

    document.body.replaceChild(updatedState.rendered, state.rendered)
    state = updatedState
  }
}

object Duck {

  private val Debug = false

  val Width = 96
  val Height = 96

  // the Duck's current state (position, velocity, orientation, sprite, etc.)

  class State private (
    val pos: Pos, val vel: Vel, val accel: Accel,
    val sprite: Duck.Sprite, val facingRight: Boolean,
    val config: State.Config = State.Config.default) {

    lazy val rendered: Div = {

      val image = document.createElement("img").asInstanceOf[Image]
      image.src = sprite.toString

      if (!facingRight) image.style.transform = "scaleX(-1)"

      div(
        position := "absolute",
        top := -pos.y,
        left := pos.x,
        width := Duck.Width,
        height := Duck.Height,
      )(image).render
    }

    def copy(
          pos: Pos = this.pos,
          vel: Vel = this.vel,
          accel: Accel = this.accel,
          sprite: Duck.Sprite = this.sprite,
          facingRight: Boolean = this.facingRight,
          config: State.Config = this.config
        ): State = {
      State(pos, vel, accel, sprite, facingRight, config)
    }

  }

  object State {

    case class Config(accel: Accel, maxSpeed: Double)

    object Config {
      val default: Config = Config(Accel(2, 10), 15)
    }

    def apply (
      pos: Pos, vel: Vel, accel: Accel,
      sprite: Duck.Sprite, facingRight: Boolean,
      config: State.Config = State.Config.default): State = {

      def clampSpeed (vel: Vel): Vel = {
        val vx = Math.max(Math.min(vel.x, config.maxSpeed), -config.maxSpeed)
        Vel(vx, vel.y)
      }

      new State(pos, clampSpeed(vel), accel, sprite, facingRight, config)
    }

  }

  // sprites used to render the Duck

  sealed trait Sprite

  object Sprite {
    private val dir = "src/main/resources/sprites"

    object Crouching extends Sprite {
      override def toString = s"$dir/crouching.png"
    }

    object Dead extends Sprite {
      override def toString = s"$dir/dead.png"
    }

    object Idle extends Sprite {
      override def toString = s"$dir/idle.png"
    }

    object Idle2 extends Sprite {
      override def toString = s"$dir/idle2.png"
    }

    object Jumping extends Sprite {
      override def toString = s"$dir/jumping.png"
    }

    object Running extends Sprite {
      override def toString = s"$dir/running.png"
    }

    object Running2 extends Sprite {
      override def toString = s"$dir/running2.png"
    }

    object Walking extends Sprite {
      override def toString = s"$dir/walking.png"
    }

    object Walking2 extends Sprite {
      override def toString = s"$dir/walking2.png"
    }
  }

  // commands the Duck understands

  sealed trait Command

  object Command {

    case object GoLeft extends Command

    case object StopLeft extends Command

    case object GoRight extends Command

    case object StopRight extends Command

    case object Jump extends Command

    case object ReRender extends Command

  }

}