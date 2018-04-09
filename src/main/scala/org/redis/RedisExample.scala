package org.redis

import java.util.concurrent.TimeUnit

import scala.util.{Failure, Success, Try}
import monix.eval.Task

import scala.collection.JavaConverters._
import monix.execution.Scheduler.Implicits.global
import org.redisson.Redisson

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object RedisExample extends App {
  val redisson = Redisson.create

  val task1 = Task {
    val lock = redisson.getFairLock("creating-new-subscription")
    Try {
      println("myLock1 task1 " + lock.isLocked)

      val res = lock.tryLock(1000L, 20000L, TimeUnit.MILLISECONDS)
      Thread.sleep(100)
      println("myLock1 task1 " + res)

    } match {
      case Success(value) =>
        println("After task1")
      case Failure(ex) =>
        lock.forceUnlock()
        ex.printStackTrace()
    }
  }.executeAsync.runAsync

  val task2 = Task {
    val lock = redisson.getFairLock("creating-new-subscription")
    Try {
      println("myLock1 task2 " + lock.isLocked)

      val res = lock.tryLock(1000L, 20000L, TimeUnit.MILLISECONDS)
      Thread.sleep(100)
      println("myLock1 task2 " + res)

    } match {
      case Success(value) =>
        println("After task2")
      case Failure(ex) =>
        lock.forceUnlock()
        ex.printStackTrace()
    }
  }.executeAsync.runAsync

  val task3 = Task {
    val lock = redisson.getFairLock("creating-new-subscription")
    Try {
      println("myLock1 task3 " + lock.isLocked)

      val res = lock.tryLock(1000L, 20000L, TimeUnit.MILLISECONDS)
      Thread.sleep(100)
      println("myLock1 task3 " + res)

    } match {
      case Success(value) =>
        println("After task3")
      case Failure(ex) =>
        lock.forceUnlock()
        ex.printStackTrace()
    }
  }.executeAsync.runAsync

  val task4 = Task {
    val lock = redisson.getFairLock("creating-new-subscription")
    Try {
      println("myLock1 task4 " + lock.isLocked)

      val res = lock.tryLock(1000L, 20000L, TimeUnit.MILLISECONDS)
      println("myLock1 task4 " + res)
      Thread.sleep(100)

    } match {
      case Success(value) =>
        println("After task4")
      case Failure(ex) =>
        lock.forceUnlock()
        ex.printStackTrace()
    }
  }.executeAsync.runAsync

  val task5 = Task {
    val lock = redisson.getFairLock("creating-new-subscription")
    Try {
      println("myLock1 task5 " + lock.isLocked)

      val res = lock.tryLock(1000L, 20000L, TimeUnit.MILLISECONDS)
      Thread.sleep(100)
      println("myLock1 task5 " + res)

    } match {
      case Success(value) =>
        println("After task5")
      case Failure(ex) =>
        lock.forceUnlock()
        ex.printStackTrace()
    }
  }.executeAsync.runAsync

  //Task.gatherUnordered(Seq(task1,task2,task3,task4,task5)).runAsync
  Await.result(task1, Duration.Inf)
  Await.result(task2, Duration.Inf)
  Await.result(task3, Duration.Inf)
  Await.result(task4, Duration.Inf)
  Await.result(task5, Duration.Inf)
  redisson.shutdown()
}
