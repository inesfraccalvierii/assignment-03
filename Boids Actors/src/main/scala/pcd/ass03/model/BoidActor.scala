package pcd.ass03.model

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import pcd.ass03.Configuration.*

import scala.util.Random


object BoidActor:

  private final case class Weights(
                                    separation: Double,
                                    alignment: Double,
                                    cohesion: Double
                                  )


  // ============================================================
  // Stato iniziale casuale (usato dal manager)
  // ============================================================

  def randomState(id: Int): BoidState =
    BoidState(
      id,
      P2d(
        MinX + Random.nextDouble() * EnvironmentWidth,
        MinY + Random.nextDouble() * EnvironmentHeight
      ),
      V2d(
        Random.nextDouble() * MaxSpeed / 2 - MaxSpeed / 4,
        Random.nextDouble() * MaxSpeed / 2 - MaxSpeed / 4
      )
    )


  def apply(initial: BoidState): Behavior[BoidMessage] =
    running(
      initial,
      Weights(SeparationWeight, AlignmentWeight, CohesionWeight)
    )


  private def running(
                       state: BoidState,
                       weights: Weights
                     ): Behavior[BoidMessage] =

    Behaviors.receiveMessage {

      // ========================================================
      // Calcola il prossimo step a partire dallo snapshot
      // ========================================================

      case ComputeNextStep(tick, snapshot, replyTo) =>

        val newState =
          nextState(state, snapshot, weights)

        replyTo ! StepDone(tick, newState)

        running(newState, weights)


      // ========================================================
      // Aggiorna i pesi
      // ========================================================

      case UpdateWeight(separation, alignment, cohesion) =>

        running(
          state,
          Weights(separation, alignment, cohesion)
        )
    }


  // ============================================================
  // Nuovo stato
  // ============================================================

  private def nextState(
                         self: BoidState,
                         snapshot: Vector[BoidState],
                         weights: Weights
                       ): BoidState =

    val nearby =
      snapshot.filter { other =>
        other.id != self.id &&
          self.position.distance(other.position) < PerceptionRadius
      }

    val newVelocity =
      limitSpeed(
        self.velocity
          .sum(separation(self.position, nearby).mul(weights.separation))
          .sum(alignment(self.velocity, nearby).mul(weights.alignment))
          .sum(cohesion(self.position, nearby).mul(weights.cohesion))
      )

    val newPosition =
      self.position
        .sum(newVelocity)
        .wrapped(MinX, MaxX, MinY, MaxY)

    self.copy(
      position = newPosition,
      velocity = newVelocity
    )


  // ============================================================
  // Limite velocità
  // ============================================================

  private def limitSpeed(velocity: V2d): V2d =
    if velocity.abs > MaxSpeed then
      velocity.getNormalized.mul(MaxSpeed)
    else
      velocity


  // ============================================================
  // Separation
  // ============================================================

  private def separation(
                          position: P2d,
                          neighbors: Vector[BoidState]
                        ): V2d =

    val close =
      neighbors.filter { n =>
        position.distance(n.position) < AvoidRadius
      }

    if close.isEmpty then
      V2d(0, 0)
    else
      close
        .map(n => position.sub(n.position))
        .foldLeft(V2d(0, 0))(_.sum(_))
        .mul(1.0 / close.size)
        .getNormalized


  // ============================================================
  // Alignment
  // ============================================================

  private def alignment(
                         velocity: V2d,
                         neighbors: Vector[BoidState]
                       ): V2d =

    if neighbors.isEmpty then
      V2d(0, 0)
    else
      neighbors
        .map(_.velocity)
        .foldLeft(V2d(0, 0))(_.sum(_))
        .mul(1.0 / neighbors.size)
        .sub(velocity)
        .getNormalized


  // ============================================================
  // Cohesion
  // ============================================================

  private def cohesion(
                        position: P2d,
                        neighbors: Vector[BoidState]
                      ): V2d =

    if neighbors.isEmpty then
      V2d(0, 0)
    else
      val center =
        P2d(
          neighbors.map(_.position.x).sum / neighbors.size,
          neighbors.map(_.position.y).sum / neighbors.size
        )

      center
        .sub(position)
        .getNormalized