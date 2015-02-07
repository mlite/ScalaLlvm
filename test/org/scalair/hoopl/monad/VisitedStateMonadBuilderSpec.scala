package org.scalair.hoopl.monad

/**
 * User: wangn
 * Date: 9/28/11
 */

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import collection.immutable.HashSet

class VisitedStateMonadBuilderSpec extends FlatSpec with ShouldMatchers {
  "VistatedStateMonadBuilder" should " work " in {
    val computation =
      for {
        x <- VisitedStateMonadBuilder._get(())
        y <- VisitedStateMonadBuilder.mark(1)
        z <- VisitedStateMonadBuilder.mark(2)
        v <- VisitedStateMonadBuilder.marked(2)
      } yield v
    computation.runState(HashSet())(()) should equal (true, HashSet(1,2))
  }
}