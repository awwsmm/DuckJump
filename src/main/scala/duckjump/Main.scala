package duckjump

import akka.actor.{ActorSystem, Props}
import duckjump.Physics.{Accel, Pos, Vel}
import duckjump.actors.Duck
import org.scalajs.dom
import org.scalajs.dom.{document, window}

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.DurationInt

object Main {

  implicit val system: ActorSystem = ActorSystem()

  implicit val executor: ExecutionContextExecutor = system.dispatcher

  private val renderEvery = 100.millis

  val ground: Int = -(window.innerHeight.toInt) + Duck.Height

  def main(args: Array[String]): Unit = {

    val duck = system.actorOf(Props(new Duck(Duck.State(Pos(300, ground), Vel.Zero, Accel.Zero))))

    document.addEventListener("DOMContentLoaded", (_: dom.Event) => {
      system.scheduler.scheduleAtFixedRate(0.millis, renderEvery, duck, Duck.Command.ReRender)
    })

    document.addEventListener("keydown", (k: dom.KeyboardEvent) => {
      k.key match {
        case "ArrowLeft" =>  duck ! Duck.Command.GoLeft
        case "ArrowRight" => duck ! Duck.Command.GoRight
        case "ArrowUp" =>    duck ! Duck.Command.Jump
        case _ =>
      }
    })

    document.addEventListener("keyup", (k: dom.KeyboardEvent) => {
      k.key match {
        case "ArrowLeft" =>  duck ! Duck.Command.StopLeft
        case "ArrowRight" => duck ! Duck.Command.StopRight
        case _ =>
      }
    })
  }

}