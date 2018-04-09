package org.validation

import cats.data.Validated._
import cats.implicits._
import cats.data._

object ValidationApp extends App {

  case class Data(email: String, phone: String)

  def validateEmail(email: String) = {
    val emailRegex =
      """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    email match {
      case e if emailRegex.findFirstMatchIn(e).isDefined =>
        Validation.success(e)
      case _ => Validation.failure(BadRequest("100", "Invalid email"))
    }
  }

  def validatePhone(phone: String) = {
    val phoneRegex = """^\+(?:[0-9] ?){6,14}[0-9]$""".r
    phone match {
      case p if phoneRegex.findFirstMatchIn(p).isDefined =>
        Validation.success(p)
      case _ => Validation.failure(BadRequest("101", "Invalid phone number"))
    }
  }

  println(
    Validation.accumulate(validateEmail("email@email.com"),
                          validatePhone("+1 1234567890123"))(Data))
  println(
    Validation.accumulate(validateEmail("email.com"),
                          validatePhone("+1 1234567890123"))(Data))
  println(
    Validation.accumulate(validateEmail("email@email.com"),
                          validatePhone("not-a-valid-phone"))(Data))
  println(
    Validation.accumulate(validateEmail("email.com"),
                          validatePhone("not-a-valid-phone"))(Data))
}
