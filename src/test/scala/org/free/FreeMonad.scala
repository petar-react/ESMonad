package org.free

import org.scalatest.FunSuite

class FreeMonad extends FunSuite {

  type UserName = String
  type Image = String

  case class User(name: UserName, photo: Image)

  sealed trait Service[A]

  case class GetUserName(userId: Long) extends Service[UserName]
  case class GetUserPhoto(userId: Long) extends Service[Image]

  import cats.free.Free
  type ServiceF[A] = Free[Service, A]

  import cats.free.Free.liftF

  def getUserName(userId: Long): ServiceF[UserName] = liftF(GetUserName(userId))
  def getUserPhoto(userId: Long): ServiceF[Image] = liftF(GetUserPhoto(userId))

  def getUser(id: Long) =
    for {
      n <- getUserName(id)
      p <- getUserPhoto(id)
    } yield User(n, p)

  test("Free monad with IdInterpreter") {
    import cats.{Id, ~>}

    object ServicePrinter extends (Service ~> Id) {
      def apply[A](fa: Service[A]): Id[A] = fa match {

        case GetUserName(userId) =>
          "Test"

        case GetUserPhoto(userId) =>
          " (°~°) "

      }
    }

    val user: Id[User] = getUser(1).foldMap(ServicePrinter)
    assert(user.photo == " (°~°) ")
  }

  test("Free monad with IdInterpreters") {
    import cats.{Id, ~>}

    object HappyService extends (Service ~> Id) {
      def apply[A](fa: Service[A]): Id[A] = fa match {

        case GetUserName(userId) =>
          "Test"

        case GetUserPhoto(userId) =>
          " ◕‿◕ "

      }
    }

    object SadService extends (Service ~> Id) {
      def apply[A](fa: Service[A]): Id[A] = fa match {

        case GetUserName(userId) =>
          "Test"

        case GetUserPhoto(userId) =>

          " ◉︵◉ "

      }
    }

    val isHappy = false
    val interpreter = if (isHappy) HappyService else SadService

    val user = getUser(1).foldMap(interpreter)
    assert(user.photo == " ◉︵◉ ")
  }

  test("Free monad with FutureInterpreter") {
    import scala.concurrent.{Future, Await}
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    import cats.~>
    import cats.implicits._

    object ServiceFuture extends (Service ~> Future) {
      def apply[A](fa: Service[A]): Future[A] = fa match {

        case GetUserName(userId) =>
          Future("Test")

        case GetUserPhoto(userId) =>

          Future(" (°~°) ")
      }
    }

    val user: Future[User] = getUser(1).foldMap(ServiceFuture)
    val r = Await.result(user, 10 seconds)
    assert(r.photo == " (°~°) ")
  }

  test("Free monad with EitherInterpreter Right") {
    import cats.~>
    import cats.implicits._

    type ErrorOr[A] = Either[String, A]

    object ServiceEither extends (Service ~> ErrorOr) {
      def apply[A](fa: Service[A]): ErrorOr[A] = fa match {

        case GetUserName(userId) =>
          "Mauricio".asRight

        case GetUserPhoto(userId) =>
          " ◕‿◕ ".asRight

      }
    }

    val user: ErrorOr[User] = getUser(1).foldMap(ServiceEither)
    user match {
      case Right(u) => assert(u.photo == " ◕‿◕ ")
    }
  }

  test("Free monad with EitherInterpreter Left") {
    import cats.~>
    import cats.implicits._

    type ErrorOr[A] = Either[String, A]

    object ServiceEither extends (Service ~> ErrorOr) {
      def apply[A](fa: Service[A]): ErrorOr[A] = fa match {

        case GetUserName(userId) =>
          "Mauricio".asRight

        case GetUserPhoto(userId) =>
          " ◉︵◉ ".asLeft

      }
    }

    val user: ErrorOr[User] = getUser(1).foldMap(ServiceEither)
    user match {
      case Left(e) => assert(e == " ◉︵◉ ")
    }
  }

  test("Free monad with OptionInterpreter") {
    import cats.~>
    import cats.implicits._

    object ServiceOption extends (Service ~> Option) {
      def apply[A](fa: Service[A]): Option[A] = fa match {

        case GetUserName(userId) =>
          "Mauricio".some

        case GetUserPhoto(userId) =>
          " (°~°) ".some

      }
    }

    val user: Option[User] = getUser(1).foldMap(ServiceOption)
    assert(user.isDefined)
  }

  test("Free monad with OptionTInterpreter") {
    import cats.data.OptionT
    import scala.concurrent.{Future, Await}
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    import cats.~>
    import cats.implicits._

    type FutureOpt[A] = OptionT[Future, A]

    object ServiceOptionT extends (Service ~> FutureOpt) {
      def apply[A](fa: Service[A]): FutureOpt[A] = fa match {

        case GetUserName(userId) =>
          OptionT(Future(Option("Mauricio")))

        case GetUserPhoto(userId) =>
          OptionT(Future(Option(" (°~°) ")))

      }
    }

    val user: FutureOpt[User] = getUser(1).foldMap(ServiceOptionT)

    /*
    user.map{ u =>
      assert( u.photo == " (°~°) " )
    }
     */

    val r: Option[User] = Await.result(user.value, 10 seconds)
    assert(r.isDefined)

  }

}
