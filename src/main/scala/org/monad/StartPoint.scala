package org.monad
import cats.instances.option._ // for Monad
import cats.instances.list._ // for Monad
import cats.syntax.applicative._

object StartPoint extends App {

  /**The monad type class is cats.Monad .
     Monad extends two other type classes:
    FlatMap , which provides the flatMap method
    and Applicative which provides pure .
    Applicative also extends Functor , which gives every Monad a
    map method
    */

  /**
    * Cats provides instances for all the monads in the standard library*/


  /**
    * The syntax for monads comes from three places:
      • cats.syntax.flatMap provides syntax for flatMap ;
      • cats.syntax.functor provides syntax for map ;
      • cats.syntax.applicative provides syntax for pure .
    */

  1.pure[Option]
  1.pure[List]
}
