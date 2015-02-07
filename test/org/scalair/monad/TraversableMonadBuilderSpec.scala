package org.scalair.monad

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

/**
 * User: wangn
 * Date: 9/24/11
 */

class TraversableMonadBuilderSpec extends FlatSpec with ShouldMatchers {
  val expectedValue = List((1,2), (1,3), (2,3), (2,4))

  "TraversableMonadBuilder " should " work with flatMap" in {
    val tl1 = TraversableMonadBuilder._close(List(1,2))
    val computation = tl1.flatMap { x =>
      val tl2 = TraversableMonadBuilder._close(List(x+1, x+2))
      tl2.flatMap { y => TraversableMonadBuilder._return((x, y)) }
    }
    computation.run(()) should equal (expectedValue)
  }

  "TraversableMonadBuilder " should " work with for-comprehension " in {
    val computation =
      for {
        x <- TraversableMonadBuilder._close(List(1,2))
        y <- TraversableMonadBuilder._close(List(x+1, x+2))
      } yield (x, y)
    computation.run(()) should equal (expectedValue)
  }
}