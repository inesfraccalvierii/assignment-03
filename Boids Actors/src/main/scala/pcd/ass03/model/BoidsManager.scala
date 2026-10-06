package pcd.ass03.model

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import pcd.ass03.view.{InitDrawer, UpdateView, ViewMessage}


sealed trait ManagerMessage


final case class Start(
                        nBoids: Int
                      ) extends ManagerMessage


// Risposta di un boid allo step "tick"
final case class StepDone(
                           tick: Long,
                           state: BoidState
                         ) extends ManagerMessage


// La view ha finito di disegnare il frame dello step "tick"
final case class UpdatedView(
                              tick: Long
                            ) extends ManagerMessage


final case class StopSimulation()
  extends ManagerMessage


final case class SuspendSimulation()
  extends ManagerMessage


final case class ResumeSimulation()
  extends ManagerMessage


final case class ChangeWeights(
                                separation: Double,
                                alignment: Double,
                                cohesion: Double
                              ) extends ManagerMessage


object BoidsManager:

  private final case class Simulation(
                                       viewActor: ActorRef[ViewMessage],
                                       boids: Vector[ActorRef[BoidMessage]],
                                       states: Vector[BoidState],
                                       suspended: Boolean
                                     )


  private enum Phase:
    case WaitingStates(received: Map[Int, BoidState])
    case WaitingView
    case Paused


  // Il tick è monotono e non si azzera mai (nemmeno dopo uno Stop):
  // così i messaggi in ritardo di simulazioni o step precedenti
  // vengono riconosciuti e scartati.

  def apply(
             viewActor: ActorRef[ViewMessage]
           ): Behavior[ManagerMessage] =
    idle(viewActor, lastTick = 0L)


  // ============================================================
  // Nessuna simulazione in corso
  // ============================================================

  private def idle(
                    viewActor: ActorRef[ViewMessage],
                    lastTick: Long
                  ): Behavior[ManagerMessage] =

    Behaviors.receive { (context, message) =>

      message match

        case Start(nBoids) =>

          val states =
            Vector.tabulate(nBoids)(BoidActor.randomState)

          val boids =
            states.map { state =>
              context.spawnAnonymous(BoidActor(state))
            }

          viewActor ! InitDrawer(nBoids)

          launchStep(
            context,
            Simulation(viewActor, boids, states, suspended = false),
            lastTick + 1
          )

        case _ =>
          Behaviors.same
    }


  // ============================================================
  // Avvio di uno step
  // ============================================================

  private def launchStep(
                          context: ActorContext[ManagerMessage],
                          sim: Simulation,
                          tick: Long
                        ): Behavior[ManagerMessage] =

    sim.boids.foreach { boid =>
      boid ! ComputeNextStep(tick, sim.states, context.self)
    }

    active(sim, tick, Phase.WaitingStates(Map.empty))


  // ============================================================
  // Simulazione in corso
  // ============================================================

  private def active(
                      sim: Simulation,
                      tick: Long,
                      phase: Phase
                    ): Behavior[ManagerMessage] =

    Behaviors.receive { (context, message) =>

      (message, phase) match

        // ------------------------------------------------------
        // Stato di un boid per lo step corrente
        // ------------------------------------------------------

        case (StepDone(t, state), Phase.WaitingStates(received)) if t == tick =>

          val updated =
            received + (state.id -> state)

          if updated.size == sim.boids.size then

            val states =
              Vector.tabulate(sim.boids.size)(updated)

            sim.viewActor ! UpdateView(
              tick,
              states.map(_.position),
              context.self
            )

            active(sim.copy(states = states), tick, Phase.WaitingView)

          else

            active(sim, tick, Phase.WaitingStates(updated))


        // ------------------------------------------------------
        // Frame disegnato: prossimo step (o pausa)
        // ------------------------------------------------------

        case (UpdatedView(t), Phase.WaitingView) if t == tick =>

          if sim.suspended then
            active(sim, tick, Phase.Paused)
          else
            launchStep(context, sim, tick + 1)


        // ------------------------------------------------------
        // Suspend / Resume
        // ------------------------------------------------------

        case (SuspendSimulation(), _) =>

          active(sim.copy(suspended = true), tick, phase)


        case (ResumeSimulation(), Phase.Paused) =>

          launchStep(context, sim.copy(suspended = false), tick + 1)


        case (ResumeSimulation(), _) =>

          // Lo step in corso non era ancora finito: basta togliere il flag
          active(sim.copy(suspended = false), tick, phase)


        // ------------------------------------------------------
        // Pesi
        // ------------------------------------------------------

        case (ChangeWeights(separation, alignment, cohesion), _) =>

          sim.boids.foreach { boid =>
            boid ! UpdateWeight(separation, alignment, cohesion)
          }

          Behaviors.same


        // ------------------------------------------------------
        // Stop
        // ------------------------------------------------------

        case (StopSimulation(), _) =>

          sim.boids.foreach(context.stop)

          idle(sim.viewActor, tick)


        // ------------------------------------------------------
        // Messaggi vecchi o fuori fase
        // ------------------------------------------------------

        case _ =>
          Behaviors.same
    }