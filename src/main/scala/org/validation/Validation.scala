package org.validation

import cats.data._

object Validation extends AccumulateArities {

  def success[T](t: T): Validation[T] = Validated.valid(t)

  def failure[T](err: Err): Validation[T] = Validated.invalidNel(err)
}


