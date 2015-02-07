package org.scalair.monad

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 9/25/11
 * Time: 11:24 AM
 */

class OptionMonadBuilderSpec extends FlatSpec with ShouldMatchers {
  val expectedValue = Some((1,2))
  "OptionMonadBuilder" should " work with flatMap" in {
    val computation =
      OptionMonadBuilder._close(Some(1)).flatMap { x =>
        OptionMonadBuilder._close(Some(x+1)).flatMap { y =>
          OptionMonadBuilder._return((x,y))
        }
      }
    computation.run(()) should equal (expectedValue)
  }
  "OptionMonadBuilder" should " work with for-comprehension" in {
    val computation =
      for {
        x <- OptionMonadBuilder._close(Some(1))
        y <- OptionMonadBuilder._close(Some(x+1))
      } yield (x, y)
    computation.run(()) should equal (expectedValue)
  }

  "OptionMonadBuilder" should " work " in {
    val computation =
      for {
        x <- OptionMonadBuilder._close(Some(1))
        y <- OptionMonadBuilder._close(None)
      } yield (x, y)
    computation.run(()) should equal (None)
  }
}