package org.monad

import cats.MonadError
import cats.instances.either._
import scala.util.Try
import cats.instances.try_._ // for MonadError

object MonadErrorExample extends App {
  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  println(monadError.pure(42))
  println(monadError.raiseError("Badness"))

  val failure = monadError.raiseError("Badness")

  monadError.handleError(failure) {
    case "Badness" =>
      monadError.pure("It's ok")
    case other =>
      monadError.raiseError("It's not ok")
  }

  monadError.ensure(monadError.pure(42))("Number too low!")(_ > 1000)


  import cats.syntax.applicative._
  // for pure
  import cats.syntax.applicativeError._ // for raiseError etc

  val success = 42.pure[ErrorOr]
  val failures = "Badness".raiseError[ErrorOr, Int]

  val exn: Throwable =
    new RuntimeException("It's all gone wrong")
  println(exn.raiseError[Try, Int])
}
