package pcd.ass03.model

import akka.actor.typed.ActorRef


// Stato di un boid in un dato step (dato immutabile, non un messaggio)
final case class BoidState(
                            id: Int,
                            position: P2d,
                            velocity: V2d
                          )


sealed trait BoidMessage


final case class ComputeNextStep(
                                  tick: Long,
                                  snapshot: Vector[BoidState],
                                  replyTo: ActorRef[ManagerMessage]
                                ) extends BoidMessage


final case class UpdateWeight(
                               separation: Double,
                               alignment: Double,
                               cohesion: Double
                             ) extends BoidMessage