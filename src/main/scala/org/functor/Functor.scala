package org.functor

import simulacrum.typeclass

//Give some effect
//Model some type of the effect with F
//Option is going to model effect of having a value or not having a value
//List is going to model effect of having multiple values
@typeclass trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)

  def as[A, B](fa: F[A], b: => B): F[B] = map(fa)(_ => b)

  def void[A](fa: F[A]): F[Unit] = as(fa, ())

  def compose[G[_]](implicit G: Functor[G]): Functor[Lambda[X => F[G[X]]]] =
    new Functor[Lambda[X => F[G[X]]]] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = {
        self.map(fga)(ga => G.map(ga)(a => f(a)))
      }
    }
}

trait FunctorLaws {

  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) =
    F.map(fa)(a => a) == fa

  def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(
      implicit F: Functor[F]) =
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
}

object Functor {

  // List type constructor, takes one parameter
  // List[A] type, produced using a type parameter

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  //Map of result of function
  //A => B as proper type
  // We take A as input but change otput throguht map function
  //It's def because we need to ask for A type
  implicit def function1Functor[X]: Functor[X => ?] = new Functor[X => ?] {
    override def map[A, B](fa: X => A)(f: A => B): X => B = fa andThen f
  }
}

object FunctorApp extends App {

  //types that take one argument we call type constructors with one argument

  //F[_] type constructor with one argument
  def bla[F[_], A](x: F[A], y:F[A]): Unit ={

  }
  val implicitList = implicitly[Functor[List]]

  val result = implicitList.map(List(1, 2, 3))(_ + 1)

  val implicitFuncFunctor = implicitly[Functor[Int => ?]]

  val resultFunction = implicitFuncFunctor.map(_ + 1)(_.toString)

  val composed = Functor[List] compose Functor[Option]

  composed.map(List(Some(1), None, Some(3)))(_ + 1)

}
