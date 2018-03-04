package org.monad

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._
//import scala.concurrent._
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.duration._

object MonadWriterApp extends App {

  /**
    * A Writer[W, A] carries two values: a log of type W and a result of type A
    */
  type Logged[A] = Writer[Vector[String], A]
  val a = 123.pure[Logged]
  println(a)

  /**
    * If we have a log and no result we can create a Writer[Unit] using the tell
    *syntax from cats.syntax.writer
    */
  println(Vector("msg1", "msg2", "msg3").tell)

  /**
    * If we have both a result and a log, we can either use Writer.apply or we can
    *use the writer syntax from cats.syntax.writer :
    */
  val aa = Writer(Vector("msg1", "msg2", "msg3"), 123)
  println(aa)

  val b = 123.writer(Vector("msg1", "msg2", "msg3"))

  println(b)

  println(a.value)
  println(a.written)
  println(b.run)

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  println(writer1)

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

  println(writer2)


  /**
    * We can transform both log and result simultaneously using bimap or mapBoth .
    *  bimap takes two func on parameters, one for the log and one for the result.
    *  mapBoth takes a single func on that accepts two parameters
    */

  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )


  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }


  val writer5 = writer1.reset

  val writer6 = writer1.swap

  println(s"factorial(3): ${factorial(3)}")
  println(s"factorialWriter(3).run: ${factorialWriter(3).run}")

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def factorialWriter(n: Int): Logged[Int] = {
    val wans =
      if (n == 0) 1.pure[Logged]
      else factorialWriter(n - 1).map(n * _)
    wans
      .map(slowly(_))
      .flatMap(ans => Vector(s"fact $n $ans").tell.map(_ => ans))
  }
}
