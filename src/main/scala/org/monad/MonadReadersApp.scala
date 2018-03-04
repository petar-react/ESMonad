package org.monad

import cats.data.Reader
import cats.syntax.applicative._

object MonadReadersApp extends App {

  case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)

  catName.run(Cat("Garfield", "lasagne"))

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello ${name}")
  greetKitty.run(Cat("Heathcliff", "junk food"))

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  greetAndFeed(Cat("Heathcliff", "junk food"))

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."

  greetAndFeed(Cat("Garfield", "lasagne"))

  case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  private val db =
    Db(Map(
         1 -> "dade",
         2 -> "kate",
         3 -> "margo"
       ),
       Map(
         "dade" -> "zerocool",
         "kate" -> "acidburn",
         "margo" -> "secret"
       ))

  println(
    s"""checkLogin(1, "zerocool"): ${checkLogin(1, "zerocool").run(db)}""")
  println(s"""checkLogin(4, "davinci"): ${checkLogin(4, "davinci").run(db)}""")

  private def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  private def checkPassword(
      username: String,
      password: String
  ): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  private def checkLogin(
      userId: Int,
      password: String
  ): DbReader[Boolean] =
    findUsername(userId).flatMap {
      case (Some(username)) => checkPassword(username, password)
      case None             => false.pure[DbReader]
    }
}
