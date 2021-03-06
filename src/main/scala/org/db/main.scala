package org.db

// Import the Slick interface for H2:

import slick.jdbc.H2Profile.api._

import scala.concurrent.Await
import scala.concurrent.duration._

object Example extends App {

  // Case class representing a row in our table:
  final case class Message(sender: String, content: String, id: Long = 0L)

  // Helper method for creating test data:
  def freshTestData = Seq(
    Message("Dave", "Hello, HAL. Do you read me, HAL?"),
    Message("HAL", "Affirmative, Dave. I read you."),
    Message("Dave", "Open the pod bay doors, HAL."),
    Message("HAL", "I'm sorry, Dave. I'm afraid I can't do that."),
    Message("Dave", "What if I say 'Pretty please'?"),
    Message("HAL", "I'm sorry, Dave. I'm afraid I can't do that.")
  )

  // Schema for the "message" table:
  final class MessageTable(tag: Tag) extends Table[Message](tag, "message") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def sender = column[String]("sender")
    def content = column[String]("content")

    def * = (sender, content, id).mapTo[Message]
  }

  // Base query for querying the messages table:
  lazy val messages = TableQuery[MessageTable]

  // An example query that selects a subset of messages:
  val halSays = messages.filter(_.sender === "HAL")

  // Create an in-memory H2 database;
  val db = Database.forConfig("chapter02")

  // Helper method for running a query in this example file:
  def exec[T](program: DBIO[T]): T = Await.result(db.run(program), 2.seconds)

  try {

    // Create the "messages" table:
    println("Creating database table")
    exec(messages.schema.create)

    // Create and insert the test data:
    println("\nInserting test data")
    exec(messages ++= freshTestData)

    // Run the test query and print the results:
    println("\nSelecting all message sender names:")
    exec(messages.map(_.sender).result) foreach { println }

    println("\nSelecting only Pretty messages:")

    type Q = Query[MessageTable, Message, Seq]

    import Columns._

    val query =
      Seq((SENDER, "Dave"), (CONTENT, "%Pretty%"))
        .foldLeft(messages.asInstanceOf[Q]) {
          case (agg, (filter, value)) =>
            filter match {
              case SENDER =>
                agg.filter(x => x.sender === value)
              case CONTENT =>
                agg.filter(x => x.content like value)
            }
        }

    println(
      exec {
        query.result
      }
    )

  } finally db.close
}

object Columns extends Enumeration {
  type Columns = Value
  val SENDER, CONTENT = Value
}
