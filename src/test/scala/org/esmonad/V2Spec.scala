package org.esmonad

import org.scalatest._

/**
  * This is an example of introducing event sourcing with creation events.
  */
class V2Spec extends FlatSpec with Matchers {
  import org.esmonad.V2._

  "The V2 object" should "be valid" in {

    val events = Seq(
      Create("123", Position.zero, North),
      Walk("123", 1),
      Turn("123", ToRight),
      Walk("123", 1),
      Turn("123", ToRight),
      Walk("123", 2),
      Turn("123", ToRight),
      Walk("123", 2),
    )
    val finalState = events.foldLeft(Option.empty[Turtle]) {
      case (state, event) => Some(Turtle.handler(state, event))
    }
    finalState shouldBe Some(Turtle("123", Position(-1, -1), West))

  }

}
