package org.free

object FreeM extends App {

  type UserName = String
  type Image = String

  case class User(name: UserName, photo: Image)

  sealed trait Service[A]

  //Business logic
  case class GetUserName(userId: Long) extends Service[UserName]
  case class GetUserPhoto(userId: Long) extends Service[Image]

  import cats.free.Free

  type ServiceF[A] = Free[Service, A]

  import cats.free.Free.liftF

  // Elavate F[A] in Free monad.
  def getUserName(userId: Long): Free[Service, UserName] =
    liftF(GetUserName(userId))
  def getUserPhoto(userId: Long): ServiceF[Image] = liftF(GetUserPhoto(userId))

  //Program
  def getUser(id: Long) =
    for {
      n <- getUserName(id)
      p <- getUserPhoto(id)
    } yield User(n, p)

  //interpreter
  // ~> natural transformation
  import cats.{Id, ~>}

  object ServicePrinter extends (Service ~> Id) { self =>
    def apply[A](fa: Service[A]): Id[A] = fa match {

      case GetUserName(userId) =>
        println(s"GetUserName $userId...")
        "Test"

      case GetUserPhoto(userId) =>
        println(s"GetUserPhoto...")
        " (°~°) "

    }
  }

  val sp: Id[User] = getUser(1).foldMap(ServicePrinter)

  println(sp)

}
