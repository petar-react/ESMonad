package org.esmonad

trait FinalModels extends FinalHandlers with FinalJournals {

  sealed trait TurtleEvent { def id: String }
  case class Create(id: String, pos: Position, dir: Direction)
      extends TurtleEvent
  case class Turn(id: String, rot: Rotation) extends TurtleEvent
  case class Walk(id: String, dist: Int) extends TurtleEvent

  case class Turtle(id: String, pos: Position, dir: Direction)

  private def withinRange(pos: Position): Boolean =
    pos.x.abs < 100 && pos.y.abs < 100

  object Turtle {
    def create(id: String,
               pos: Position,
               dir: Direction): Either[String, TurtleEvent] =
      if (withinRange(pos)) Right(Create(id, pos, dir))
      else Left("Too far away")

    def turn(rot: Rotation)(turtle: Turtle): Either[String, TurtleEvent] =
      Right(Turn(turtle.id, rot))

    def walk(dist: Int)(turtle: Turtle): Either[String, TurtleEvent] = {
      val newPos = turtle.pos.move(turtle.dir, dist)
      if (withinRange(newPos)) Right(Walk(turtle.id, dist))
      else Left("Too far away")
    }

    implicit val handler = EventHandler[Turtle, TurtleEvent] {
      case (None, Create(id, pos, dir)) => Turtle(id, pos, dir)
      case (Some(t), Turn(id, rot)) if id == t.id =>
        t.copy(dir = t.dir.rotate(rot))
      case (Some(t), Walk(id, dist)) if id == t.id =>
        t.copy(pos = t.pos.move(t.dir, dist))
    }

  }

  implicit object TurtleJournal
      extends DefaultJournal[Turtle, TurtleEvent](Turtle.handler, _.id)
}
