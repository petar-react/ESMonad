package org.validation.examples

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object ValidationWithFuturesEx1 extends App {

  import cats._
  import cats.data._
  import cats.instances.future._
  import cats.instances.list._
  import cats.syntax.either._
  import cats.syntax.traverse._

  trait Error
  type Errors = NonEmptyList[Error]

  type Result[A] = Either[Errors, A]

  // Result
  case object AnError extends Error

  def aOrError: Result[String] = "a".asRight
  def bOrError: Result[Int] = NonEmptyList.of(AnError).asLeft

  // Future Result
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._

  def futAOrError: Future[Result[String]] = Future.successful("a".asRight)
  def futBOrError: Future[Result[Int]] =
    Future.successful(NonEmptyList.of(AnError).asLeft)
  def futBOrErrorS: Future[Result[Int]] =
    Future.successful(3.asRight)

  type ResultWithErrorAcc[A] = Validated[Errors, A] // to avoid lambda type
  type FutResultWithErrorAcc[A] = Future[Validated[Errors, A]] // to avoid lambda type

  implicit val instanceFutureValidatedApplicative =
    Applicative[Future].compose[ResultWithErrorAcc]

  val res2 = for {
    a <- EitherT(futAOrError)
    b <- EitherT {
      Applicative[FutResultWithErrorAcc]
        .product(futAOrError.map(_.toValidated), futBOrError.map(_.toValidated))
        .map { _.toEither }
    }
    c <- EitherT {
      List(futAOrError.map(_.toValidated), futAOrError.map(_.toValidated))
        .sequence[FutResultWithErrorAcc, String] // does not compile without explicit type
        .map(_.toEither)
    }
  } yield (a, b, c)

  val res3: Future[Result[(String, (String, Int), List[String])]] = res2.value

  println(Await.result(res3, Duration.Inf))
}
