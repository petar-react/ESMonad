package org.validation.examples

import cats.data.Validated.Valid
import org.validation.ValidationApp.Data

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ValidationWithFuturesEx2 extends App {
  import cats._, cats.implicits._, cats.data._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import cats.Semigroup
  import scala.concurrent.Future
  import cats.syntax._

  implicit class EnrichedFuture[A](future: Future[A]) {
    def toValidatedNel: Future[ValidatedNel[Throwable, A]] = {
      future.map(Validated.valid).recover {
        case e =>
          Validated.invalidNel(e)
      }
    }
  }

  def future1: Future[Int] = Future {
    println("future 1 is executing!")
    1
  }

  def future2: Future[Int] = Future {
    println("future 2 is executing!")
    throw new Exception("Future 2 failed")
  }

  def future3: Future[Int] = Future {
    println("future 3 is executing!")
    throw new Exception("Future 3 failed")
  }

  def future4: Future[Int] = Future {
    println("future 4 is executing!")
    4
  }


  val res =
    (future1.toValidatedNel,
     future2.toValidatedNel,
     future3.toValidatedNel,
     future4.toValidatedNel)
      .mapN {
        case (x: Valid[Int], y: Valid[Int], c: Valid[Int], d: Valid[Int]) =>
          println("Hello " + x + " " + y + " " + c + " " + d)
          Data("", "")
        case _ => println("Invalid ")
      }

  println("res " + Await.result(res, Duration.Inf))

  val res1 = List(future1, future2, future3, future4).traverse(_.toValidatedNel)

  println("res1 " + Await.result(res1, Duration.Inf))

}
