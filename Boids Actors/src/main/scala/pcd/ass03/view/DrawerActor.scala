package pcd.ass03.view

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import pcd.ass03.Configuration
import pcd.ass03.model.{ManagerMessage, P2d, UpdatedView}

import scala.concurrent.duration.*
import scala.swing.{Graphics2D, Panel}


sealed trait DrawMessage

final case class DrawBoids(
                            tick: Long,
                            positions: Vector[P2d],
                            replyTo: ActorRef[ManagerMessage]
                          ) extends DrawMessage


// ============================================================
// Pannello dei boid
// ============================================================
// Viene creato dal ViewActor e passato al Drawer: il pannello
// mostrato nella finestra è sempre quello che viene ridisegnato.
// I campi sono @volatile perché scritti dall'attore e letti dall'EDT.

final class BoidsPanel(nBoids: Int) extends Panel:

  @volatile private var positions: Vector[P2d] = Vector.empty
  @volatile private var frameRate: Int = 0

  background = java.awt.Color.WHITE
  opaque = true


  def update(
              newPositions: Vector[P2d],
              newFrameRate: Int
            ): Unit =
    positions = newPositions
    frameRate = newFrameRate
    repaint() // thread-safe in Swing


  override protected def paintComponent(g: Graphics2D): Unit =

    super.paintComponent(g)

    val currentPositions = positions

    val w = size.width
    val h = size.height

    val xScale = w.toDouble / Configuration.EnvironmentWidth
    val yScale = h.toDouble / Configuration.EnvironmentHeight

    g.setColor(java.awt.Color.BLUE)

    currentPositions.foreach { pos =>
      val px = (w / 2.0 + pos.x * xScale).toInt
      val py = (h / 2.0 - pos.y * yScale).toInt
      g.fillOval(px - 2, py - 2, 5, 5)
    }

    g.setColor(java.awt.Color.BLACK)
    g.drawString(s"Num. Boids: $nBoids", 10, 25)
    g.drawString(s"Framerate: $frameRate", 10, 40)


// ============================================================
// Drawer
// ============================================================

object DrawerActor:

  private val MaxFrameRate = 25
  private val FramePeriodMillis = 1000L / MaxFrameRate


  def apply(panel: BoidsPanel): Behavior[DrawMessage] =
    Behaviors.setup { context =>
      drawing(panel, System.currentTimeMillis())
    }


  // stepStart = istante in cui il manager ha potuto avviare lo step
  private def drawing(
                       panel: BoidsPanel,
                       stepStart: Long
                     ): Behavior[DrawMessage] =

    Behaviors.receive { (context, message) =>

      message match

        case DrawBoids(tick, positions, replyTo) =>

          val now = System.currentTimeMillis()
          val workTime = now - stepStart

          // Se lo step è stato più veloce del periodo di frame,
          // ritardo la risposta al manager per non superare i 25 fps
          val delay =
            math.max(0L, FramePeriodMillis - workTime)

          val frameRate =
            (1000.0 / math.max(1L, workTime + delay)).toInt

          panel.update(positions, frameRate)

          if delay > 0 then
            context.scheduleOnce(delay.millis, replyTo, UpdatedView(tick))
          else
            replyTo ! UpdatedView(tick)

          drawing(panel, now + delay)
    }