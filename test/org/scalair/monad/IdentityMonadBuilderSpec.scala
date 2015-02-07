package org.scalair.monad

import org.scalair.ScalairFeatureSpecTrait


/**
 * User: wangn
 * Date: 9/24/11
 */

class IdentityMonadBuilderSpec extends ScalairFeatureSpecTrait  {
  val builder = IdentityMonadBuilder

  feature("Basic Functions") {
    scenario("_liftM") {
      val converter = builder._liftM[String, List[String]] {x:String  => List(x) }
      val x = builder._return("x")
      when("the original value is " + x)
      val y = converter (x)
      then("the converted value is " + y)
      y.run(()) should equal (List("x"))
    }

    scenario("_liftM2") {
      val converter = builder._liftM2[Int,Int,Int] { x:Int => y:Int => x+y }
      val x = builder._return(1)
      val y = builder._return(2)
      when("x is " + x)
      and("y is " + y)
      val z = converter(x)(y)
      then("the converted value is " + z)
      z.run(()) should equal (3)
    }
  }

  feature("Advanced Features") {
    scenario("for-comprehension") {
      val computation =
        for {
          x <- builder._return(1)
          y <- builder._return(x+1)
        } yield (x, y)
      computation.run(()) should equal (1,2)
    }
  }
}