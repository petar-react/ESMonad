package org.monad

import cats.data.State

object MonadStateExample extends App {

  /**
    * State is a func on that does two things:
    * • transforms an input state to an output state;
    * • computes a result.
    */
  // Get the state and the result:
  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  // Get the state and the result:
  val (state, result) = a.run(10).value

  // Get the state, ignore the result:
  val stateS = a.runS(10).value

  // Get the result, ignore the state:
  val resultR = a.runA(10).value

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (stateC, resultC) = both.run(20).value

  println(s"State $stateC and Result $resultC")

  /**
    * get extracts the state as the result;
    * set updates the state and returns unit as the result;
    * pure ignores the state and returns a supplied result;
    * inspect extracts the state via a transforma on func on;
    * modify updates the state using an update func on.
    */
  val getDemo = State.get[Int]
  getDemo.run(10).value
  val setDemo = State.set[Int](30)
  setDemo.run(10).value
  val pureDemo = State.pure[Int, String]("Result")
  pureDemo.run(10).value
  val inspectDemo = State.inspect[Int, String](_ + "!")
  inspectDemo.run(10).value
  val modifyDemo = State.modify[Int](_ + 1)
  modifyDemo.run(10).value

  import State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

}

object PostOrderCalculatorApp extends App {

  val program1 = for {
    _ <- evalOne("10")
    _ <- evalOne("2")
    result <- evalOne("-")
  } yield result
  println(s"program1: ${program1.run(Nil).value}")

  val program2 = evalAll(List("1", "2", "+", "3", "*"))
  println(s"program2: ${program2.run(Nil).value}")

  val program3 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    result <- evalOne("*")
  } yield result
  println(s"program3: ${program3.run(Nil).value}")

  type CalcState[A] = State[List[Int], A]

  private def evalAll(input: List[String]): CalcState[Int] =
    input match {
      case hd :: Nil => evalOne(hd)
      case hd :: tl  => evalOne(hd).flatMap(_ => evalAll(tl))
      case _ =>
        State[List[Int], Int] { oldStack =>
          (oldStack, 0)
        }
    }

  private def evalOne(sym: String): CalcState[Int] = {
    State[List[Int], Int] { oldStack =>
      def op(f: (Int, Int) => Int): (List[Int], Int) = {
        val (n2 :: n1 :: rest) = oldStack
        val r = f(n1, n2)
        (r :: rest, r)
      }
      sym match {
        case "+" => op(_ + _)
        case "-" => op(_ - _)
        case "*" => op(_ * _)
        case "/" => op(_ / _)
        case _ =>
          val r = sym.toInt
          (r :: oldStack, r)
      }
    }
  }
}
