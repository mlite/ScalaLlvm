package org.scalair.monad

import org.scalair.ScalairFeatureSpecTrait

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 10/2/11
 * Time: 1:49 PM
 * To change this template use File | Settings | File Templates.
 */

class StateMonadBuilderTraitSpec extends ScalairFeatureSpecTrait {
  val builder = new StateMonadBuilderTrait {
     type S = Int
  }
  val randomInt = random.nextInt()

  feature("Basic Functions") {
    scenario("_get") {
      val computation = builder._get(())
      val tmp = computation.run(randomInt)
      then ("builder._get(()).run(" + randomInt + ") is " + tmp)
      tmp must be (randomInt, randomInt)
    }

    scenario("_liftM") {
      val converter = builder._liftM[String,List[String]] { x:String => List(x) }
      val c1 = builder._return("hello")
      val tmp1 = c1.run(randomInt)
      given("the original value is " + tmp1)
      when("the converter is {x:String => List(x) }")
      val c2 = converter(c1)
      val tmp2 = c2.run(randomInt)
      then ("liftedM is " + tmp2)
      tmp2 must be (List("hello"),randomInt)
    }

    scenario("_modify") {
      val computation = builder._modify(_+1)
      val tmp = computation.run(randomInt)
      then ("builder._modify(_+1).run(" + randomInt + ") is " + tmp)
      tmp must be ((), randomInt+1)
    }

    scenario("_return") {
      val computation = builder._return("x")
      val tmp = computation.run(randomInt)
      then ("builder._return(\"x\").run(" + randomInt + ") is " + tmp)
      tmp must be ("x", randomInt)
    }

    scenario("_set") {
      val computation = builder._set(randomInt)
      val tmp = computation.run(1)
      then("builder._set(" + randomInt + ").run(1) is " + tmp)
      tmp must be ((),randomInt)
    }
  }
}