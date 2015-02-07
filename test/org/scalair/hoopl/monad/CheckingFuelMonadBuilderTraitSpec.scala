package org.scalair.hoopl.monad

import org.scalair.ScalairFeatureSpecTrait

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 10/1/11
 * Time: 9:06 PM
 * To change this template use File | Settings | File Templates.
 */

class CheckingFuelMonadBuilderTraitSpec extends ScalairFeatureSpecTrait {
  val builder = new CheckingFuelTmonadBuilderTrait {
    override type MonadBuilderType = SimpleUniqueMonadBuilder.type
    override val monadBuilder = SimpleUniqueMonadBuilder
  }

  feature("CheckingFuelMonadBuilderTraitSpec") {
    scenario("_return") {
      val computation = builder._return("x")
      val tmp = builder.runWithFuel(1)(computation).runState(LazyList(1))(())
      then("_return(1) is " + tmp)
      tmp._1 must be ("x")
    }
    scenario("LazyList is large enough") {
      val computation = for {
        x <- builder.freshUnique
        y <- builder.freshUnique
        z <- builder.freshUnique
        u <- builder.withFuel(Some("x"))
        v <- builder.withFuel(Some("z"))
      } yield (x, y, z, u, v)
      val tmp = builder.runWithFuel(1)(computation).runState(LazyList(1))(())
      tmp._1 must be (1,2,3,Some("x"),None)
    }

    scenario("restart") {
      val computation = for {
        _ <- builder.freshUnique
        _ <- builder.withFuel(Some(List()))
        c <- builder.checkpoint
        x <- builder.freshUnique
        y <- builder.freshUnique
        z <- builder.freshUnique
        u <- builder.withFuel(Some("x"))
        v <- builder.withFuel(Some("z"))
        _ <- builder.restart(c)
        x1 <- builder.freshUnique
        y1 <- builder.freshUnique
        z1 <- builder.freshUnique
        u1 <- builder.withFuel(Some("x"))
        v1 <- builder.withFuel(Some("z"))
      } yield ((x, y, z, u, v), (x1, y1, z1, u1, v1))
      val tmp = builder.runWithFuel(3)(computation).runState(LazyList(1))(())
      then("first " + tmp._1._1)
      then("restart " + tmp._1._2)
      tmp._1._1 must be (tmp._1._2)
    }

    scenario("continue") {
      val computation = for {
        c <- builder.checkpoint
        x <- builder.freshUnique
        y <- builder.freshUnique
        z <- builder.freshUnique
        u <- builder.withFuel(Some("x"))
        v <- builder.withFuel(Some("z"))
        //_ <- builder.restart(c)
        x1 <- builder.freshUnique
        y1 <- builder.freshUnique
        z1 <- builder.freshUnique
        u1 <- builder.withFuel(Some("x"))
        v1 <- builder.withFuel(Some("z"))
      } yield (x, y, z, u, v, x1, y1, z1, u1, v1)
      val tmp = builder.runWithFuel(1)(computation).runState(LazyList(1))(())
      tmp._1 must be (1,2,3,Some("x"),None,4,5,6,None,None)
    }
  }
}