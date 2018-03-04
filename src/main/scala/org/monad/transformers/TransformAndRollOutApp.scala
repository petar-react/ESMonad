package org.monad.transformers

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object MonadTransofrmersExample extends App {
  import cats.data.Writer
  import cats.instances.list._
  // for Monad
  import cats.syntax.applicative._

  type ListOption[A] = OptionT[List, A]
  val result12: ListOption[Int] = OptionT(List(Option(10)))
  val result23: ListOption[Int] = 32.pure[ListOption]

  type Logged[A] = Writer[List[String], A]
  // Methods generally return untransformed stacks:
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None      => Writer(List(s"Failed on $str"), None)
    }
  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    result.value
  }
  // This approach doesn't force OptionT on other users' code:
  val result1 = addAll("1", "2", "3")
  val result2 = addAll("1", "a", "3")

  println(result1)
  println(result2)
}

object TransformAndRollOutApp extends App {

  type Response[A] = EitherT[Future, String, A]

  private final val PowerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  show(s"""getPowerLevel("Jazz")""", getPowerLevel("Jazz"))
  show(s"""getPowerLevel("Nobby")""", getPowerLevel("Nobby"))

  show(s"""canSpecialMove("Jazz", "Hot Rod")""",
       canSpecialMove("Jazz", "Hot Rod"))
  show(s"""canSpecialMove("Jazz", "Bumblebee")""",
       canSpecialMove("Jazz", "Bumblebee"))
  show(s"""canSpecialMove("Jazz", "Nobby")""", canSpecialMove("Jazz", "Nobby"))

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))

  private def show[A](label: String, f: => Response[A]): Unit = {
    val result = Await.result(f.value, 1.second)
    println(s"$label: $result}")
  }

  private def getPowerLevel(autobot: String): Response[Int] =
    PowerLevels.get(autobot) match {
      case Some(powerLevel) =>
        powerLevel.pure[Response]
      case None =>
        EitherT.left(Future(s"$autobot unreachable"))
    }

  private def canSpecialMove(
      ally1: String,
      ally2: String
  ): Response[Boolean] =
    for {
      x <- getPowerLevel(ally1)
      y <- getPowerLevel(ally2)
    } yield x + y > 15

  private def tacticalReport(
      ally1: String,
      ally2: String
  ): String =
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Left(msg)    => s"comms error: $msg"
      case Right(true)  => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }
}
