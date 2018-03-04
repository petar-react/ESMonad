package org.monad

import cats.syntax.either._ // for map, flatMap, asRight etc.

object EitherMonad extends App {

  val a: Either[String, Int] = 3.asRight[String]
  val b: Either[String, Int] = 4.asRight[String]

  println(a)
  println(b)

  val result = for {
    x <- a
    y <- b
  } yield x * x + y * y

  println(result)

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }

  println(countPositive(List(1, 2, 3)))
  println(countPositive(List(1, -2, 3)))

  /**
    * cats.syntax.either adds some useful extension methods to the Either
      companion object.
    */
  println(Either.catchOnly[NumberFormatException]("foo".toInt))
  println(Either.catchNonFatal(sys.error("Badness")))

  /**
    * Cats provide methods for creating an Either from other data types
    */
  println(Either.fromTry(scala.util.Try("foo".toInt)))
  println(Either.fromOption[String, Int](None, "Badness"))

  println("Error".asLeft[Int].getOrElse(0))
  println("Error".asLeft[Int].orElse(2.asRight[String]))
  println((-1).asRight[String].ensure("Must be non-negative!")(_ > 0))

  "error".asLeft[Int].recover {
    case str: String => -1
  }

  "error".asLeft[Int].recoverWith {
    case str: String => Right(-1)
  }

  "foo".asLeft[Int].leftMap(_.reverse)

  6.asRight[String].bimap(_.reverse, _ * 7)
  "bar".asLeft[Int].bimap(_.reverse, _ * 7)

  123.asRight[String]
  123.asRight[String].swap

  sealed trait LoginError extends Product with Serializable
  final case class UserNotFound(username: String) extends LoginError
  final case class PasswordIncorrect(username: String) extends LoginError

  case object UnexpectedError extends LoginError
  case class User(username: String, password: String)
  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")
      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")
      case UnexpectedError =>
        println(s"Unexpected error")
    }

  val result1: LoginResult = User("dave", "passw0rd").asRight
  val result2: LoginResult = UserNotFound("dave").asLeft

  println(result1.fold(handleError, println))
  println(result2.fold(handleError, println))

}
