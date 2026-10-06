package pcd.ass03.view

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import pcd.ass03.model.*

import scala.swing.*
import scala.swing.event.{ButtonClicked, ValueChanged}


// ============================================================
// Messaggi ViewActor
// ============================================================

sealed trait ViewMessage

final case class SendManager(
                              manager: ActorRef[ManagerMessage]
                            ) extends ViewMessage

final case class InitDrawer(
                             nBoids: Int
                           ) extends ViewMessage

final case class UpdateView(
                             tick: Long,
                             positions: Vector[P2d],
                             replyTo: ActorRef[ManagerMessage]
                           ) extends ViewMessage

final case class Stop()
  extends ViewMessage

final case class SuspendResume()
  extends ViewMessage


object ViewActor:

  private val Width: Int = 1000
  private val Height: Int = 800

  private val SliderDefaultValue = 10
  private val SliderFactor = 0.1

  private val SuspendText = "Suspend"
  private val ResumeText = "Resume"


  // ============================================================
  // Componenti Swing
  // ============================================================

  private val frame: MainFrame =
    new MainFrame:
      title = "Boids Simulation"

  private val separationSlider = makeSlider
  private val alignmentSlider = makeSlider
  private val cohesionSlider = makeSlider

  private val sliders =
    List(separationSlider, alignmentSlider, cohesionSlider)

  private val stopButton = new Button("Stop")
  private val suspendResumeButton = new Button(SuspendText)


  // ============================================================
  // Attesa del manager
  // ============================================================

  def apply(): Behavior[ViewMessage] =

    Behaviors.receive { (context, message) =>

      message match

        case SendManager(manager) =>

          context.spawn(
            EnteringPanelActor(manager),
            "entering-panel"
          )

          addListeners(context.self, manager)

          Swing.onEDT {
            showPanel(EnteringPanelActor.panel)
            frame.pack()
            frame.centerOnScreen()
            frame.visible = true
          }

          ready(manager, drawer = None, suspended = false)

        case _ =>
          Behaviors.same
    }


  // ============================================================
  // View operativa
  // ============================================================

  private def ready(
                     manager: ActorRef[ManagerMessage],
                     drawer: Option[ActorRef[DrawMessage]],
                     suspended: Boolean
                   ): Behavior[ViewMessage] =

    Behaviors.receive { (context, message) =>

      message match

        // ====================================================
        // Inizializzazione Drawer
        // ====================================================

        case InitDrawer(nBoids) =>

          drawer.foreach(context.stop)

          // Il pannello viene creato QUI e passato al drawer:
          // nessuna race condition su quale pannello è mostrato
          val boidsPanel = BoidsPanel(nBoids)

          val newDrawer =
            context.spawnAnonymous(DrawerActor(boidsPanel))

          Swing.onEDT {
            suspendResumeButton.text = SuspendText
            sliders.foreach(_.value = SliderDefaultValue)
            showPanel(simulationPanel(boidsPanel))
            frame.size = new Dimension(Width, Height)
            frame.centerOnScreen()
          }

          ready(manager, Some(newDrawer), suspended = false)


        // ====================================================
        // Nuovo frame da disegnare
        // ====================================================

        case UpdateView(tick, positions, replyTo) =>

          drawer.foreach { d =>
            d ! DrawBoids(tick, positions, replyTo)
          }

          Behaviors.same


        // ====================================================
        // Stop
        // ====================================================

        case Stop() =>

          drawer.foreach(context.stop)

          manager ! StopSimulation()

          Swing.onEDT {
            showPanel(EnteringPanelActor.panel)
            frame.pack()
            frame.centerOnScreen()
          }

          ready(manager, None, suspended = false)


        // ====================================================
        // Suspend / Resume
        // ====================================================

        case SuspendResume() =>

          if suspended then
            manager ! ResumeSimulation()
          else
            manager ! SuspendSimulation()

          val newText =
            if suspended then SuspendText else ResumeText

          Swing.onEDT {
            suspendResumeButton.text = newText
          }

          ready(manager, drawer, !suspended)


        case SendManager(_) =>
          Behaviors.same
    }


  // ============================================================
  // Pannello simulazione
  // ============================================================

  private def simulationPanel(
                               boidsPanel: BoidsPanel
                             ): BorderPanel =

    val buttonsPanel =
      new FlowPanel(stopButton, suspendResumeButton)

    val slidersPanel =
      new FlowPanel(
        new Label("Separation"), separationSlider,
        new Label("Alignment"), alignmentSlider,
        new Label("Cohesion"), cohesionSlider
      )

    new BorderPanel:
      layout(buttonsPanel) = BorderPanel.Position.North
      layout(boidsPanel) = BorderPanel.Position.Center
      layout(slidersPanel) = BorderPanel.Position.South


  // Da chiamare solo sull'EDT
  private def showPanel(panel: Component): Unit =
    frame.contents = panel
    frame.peer.revalidate()
    frame.repaint()


  // ============================================================
  // Slider
  // ============================================================

  private def makeSlider: Slider =

    new Slider():

      orientation = Orientation.Horizontal

      min = 0
      max = 20

      value = SliderDefaultValue

      majorTickSpacing = 10
      minorTickSpacing = 1

      paintTicks = true
      paintLabels = true

      labels =
        Map(
          min -> Label("0"),
          SliderDefaultValue -> Label("1"),
          max -> Label("2")
        )


  // ============================================================
  // Listener (registrati una sola volta)
  // ============================================================
  // Le reaction girano sull'EDT: inviano solo messaggi (! è thread-safe)

  private def addListeners(
                            self: ActorRef[ViewMessage],
                            manager: ActorRef[ManagerMessage]
                          ): Unit =

    stopButton.reactions += {
      case _: ButtonClicked => self ! Stop()
    }

    suspendResumeButton.reactions += {
      case _: ButtonClicked => self ! SuspendResume()
    }

    sliders.foreach { slider =>
      slider.reactions += {
        case _: ValueChanged =>
          manager ! ChangeWeights(
            separationSlider.value * SliderFactor,
            alignmentSlider.value * SliderFactor,
            cohesionSlider.value * SliderFactor
          )
      }
    }