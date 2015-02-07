package org.scalair.hoopl.monad

/**
 * User: wangn
 * Date: 9/28/11
 */

import org.scalair.ScalairFeatureSpecTrait

class SimpleUniqueMonadBuilderSpec extends ScalairFeatureSpecTrait {
  val builder = SimpleUniqueMonadBuilder
  val computation =
    for {
      x <- builder.freshUnique
      y <- builder.freshUnique
    } yield (x, y)

  feature("SimpleUniqueMonadBuilder") {
    scenario("LazyList is large enough") {
      given("LazyList[1,3]")
      when("two fresh elements are taken out")
      val tmp = computation.runState(LazyList(1, 3))(())
      then("the result must be (1,2)")
      tmp._1 must be (1,2)
    }

    scenario("LayzList is not large enough") {
      given("LayzList[1,2)")
      when("two fresh elements are taken out")
      then("exception should be thrown")
      evaluating { computation.runState(LazyList(1,2))(()) } must produce [Exception]
    }

    scenario("Checkpoint restart the count") {
      given("Lazy")
      val computation = for {
        x <- builder.checkpoint
        y <- builder.freshUnique
        z <- builder.freshUnique
        x1 <- builder.restart(x)
        y1 <- builder.freshUnique
        z1 <- builder.freshUnique
      } yield (y,z, y1, z1)

      val tmp = computation.runState((LazyList(1,3)))(())
      tmp._1 must be (1,2,1,2)
    }


    scenario("runSimpleUniqueMonad can run forever") {
      given("Lazy")
      val computation = for {
        x <- builder.freshUnique
        y <- builder.freshUnique
        z <- builder.freshUnique
        x1 <- builder.freshUnique
        y1 <- builder.freshUnique
        z1 <- builder.freshUnique
      } yield z1

      val tmp = builder.runSimpleUniqueMonad(computation)
      tmp must be (5)
    }
  }
}
