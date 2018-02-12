package org.free

import org.scalatest.FunSuite

class ComposingFreeMonads extends FunSuite {

  type UserName = String
  type Image = String

  case class User(name: UserName, photo: Image)

  /**
    * Service user
    */
  sealed trait Service[A]
  case class GetUserName(userId: Long) extends Service[UserName]
  case class GetUserPhoto(userId: Long) extends Service[Image]

  /**
    * Log
    */
  sealed trait Log[A]
  case class Info(msg: String) extends Log[Unit]
  case class Error(msg: String) extends Log[Unit]

  import cats.data.EitherK
  type CatsApp[A] = EitherK[Service, Log, A]

  import cats.free.Free

  import cats.InjectK

  class Services[F[_]](implicit I: InjectK[Service, F]) {
    def getUserNameI(userId: Long): Free[F, UserName] =
      Free.inject[Service, F](GetUserName(userId))
    def getUserPhotoI(userId: Long): Free[F, Image] =
      Free.inject[Service, F](GetUserPhoto(userId))
  }
  object Services {
    implicit def services[F[_]](implicit I: InjectK[Service, F]): Services[F] =
      new Services[F]
  }

  class Logs[F[_]](implicit I: InjectK[Log, F]) {
    def infoI(msg: String): Free[F, Unit] = Free.inject[Log, F](Info(msg))
    def errorI(msg: String): Free[F, Unit] = Free.inject[Log, F](Error(msg))
  }
  object Logs {
    implicit def logs[F[_]](implicit I: InjectK[Log, F]): Logs[F] = new Logs[F]
  }

  test("Composing Free monads") {

    def program(id: Long)(implicit O: Services[CatsApp],
                          L: Logs[CatsApp]): Free[CatsApp, User] = {
      import O._
      import L._

      for {
        n <- getUserNameI(id)
        _ <- infoI(" ~> ")
        p <- getUserPhotoI(id)
      } yield User(n, p)

    }

    import cats.{Id, ~>}

    // I 1
    object ServiceUser extends (Service ~> Id) {
      def apply[A](fa: Service[A]): Id[A] = fa match {

        case GetUserName(userId) =>
          "Test"

        case GetUserPhoto(userId) =>
          " (°~°) "

      }
    }

    // I 2
    object LogPrinter extends (Log ~> Id) {
      def apply[A](fa: Log[A]): Id[A] = fa match {

        case Info(msg) =>
          println(s"Info: $msg")

        case Error(msg) =>
          println(s"Error: $msg")

      }
    }

    val interpreter: CatsApp ~> Id = ServiceUser or LogPrinter
    val user: Id[User] = program(1).foldMap(interpreter)

    assert(user.photo == " (°~°) ")

  }

}
