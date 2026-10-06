package pcd.ass03.view

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import pcd.ass03.model.{ManagerMessage, Start}

import scala.swing.*
import scala.swing.event.ButtonClicked
import scala.util.{Success, Try}


// Il testo viene letto sull'EDT al momento del click
final case class Click(text: String)


object EnteringPanelActor:

  private val TextFieldColumns = 25
  private val FontSize = 14
  private val XLayoutAlignment = 0.5


  private val field =
    new TextField(TextFieldColumns):
      horizontalAlignment = Alignment.Center
      maximumSize = new Dimension(200, 30)
      xLayoutAlignment = XLayoutAlignment


  private val startButton =
    new Button("Start"):
      xLayoutAlignment = XLayoutAlignment
      font = new Font(Font.SansSerif, Font.Style.Bold.id, FontSize)


  private val label =
    new Label("Number of boids:"):
      xLayoutAlignment = XLayoutAlignment
      font = new Font(Font.SansSerif, Font.Style.Bold.id, FontSize)


  private val centerPanel =
    new BoxPanel(Orientation.Vertical):
      contents ++= List(label, field, startButton)


  // Il pannello viene solo costruito qui: è il ViewActor a mostrarlo
  val panel: BorderPanel =
    new BorderPanel:
      layout(centerPanel) = BorderPanel.Position.Center


  // Va creato UNA sola volta (lo fa il ViewActor), così la reaction
  // sul bottone non si accumula a ogni Stop
  def apply(
             manager: ActorRef[ManagerMessage]
           ): Behavior[Click] =

    Behaviors.setup { context =>

      val self = context.self

      startButton.reactions += {
        case _: ButtonClicked => self ! Click(field.text)
      }

      Behaviors.receiveMessage {

        case Click(text) =>

          Try(text.trim.toInt) match

            case Success(nBoids) if nBoids > 0 =>
              manager ! Start(nBoids)

            case _ =>
              Console.err.println(
                "Illegal value inserted! Positive integer is requested."
              )

          Behaviors.same
      }
    }