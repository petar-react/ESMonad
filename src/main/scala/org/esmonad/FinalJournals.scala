package org.esmonad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FinalJournals {
  self: FinalHandlers =>

  trait WriteJournal[EVENT] {
    def persist(events: Seq[EVENT]): Future[Unit]
  }

  trait Hydratable[STATE] {
    def hydrate(id: String): Future[Option[STATE]]
  }

  class DefaultJournal[STATE, EVENT](
      handler: EventHandler[STATE, EVENT],
      eventID: EVENT => String
  ) extends WriteJournal[EVENT]
      with Hydratable[STATE] {
    private var journal = Seq.empty[EVENT]

    override def persist(event: Seq[EVENT]): Future[Unit] = Future {
      synchronized { journal = journal ++ event }
    }
    def journal(id: String): Future[Seq[EVENT]] = Future {
      synchronized { journal.filter(event => eventID(event) == id) }
    }
    override def hydrate(id: String): Future[Option[STATE]] =
      journal(id).map(_.foldLeft(Option.empty[STATE])(handler))
  }

  def hydrate[STATE: Hydratable](id: String): Future[Option[STATE]] =
    implicitly[Hydratable[STATE]].hydrate(id)

  def persist[EVENT: WriteJournal](events: Seq[EVENT]): Future[Unit] =
    implicitly[WriteJournal[EVENT]].persist(events)

}
