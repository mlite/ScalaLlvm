package org.scalair.monad


import org.scalair.ScalairFeatureSpecTrait

/**
 * User: wangn
 * Date: 9/26/11
 */

class StateTidentityMonadBuilderTraitSpec extends ScalairFeatureSpecTrait {
  val builder = new StateTidentityMonadBuilderTrait {
    type S = Int
  }
  val randomInt = random.nextInt()

  feature("Basic Functions") {

    scenario("_closeT") {
      val computation = builder._closeT {
        // x is bound to the first parameter
        // passed to runState
        x => builder._fromReturn((x, x + 1))
      }

      val tmp0 = computation.run(randomInt)
      val tmp = tmp0.run(())
      tmp should equal (randomInt, randomInt+1)
    }

    scenario("_get") {
      val computation = builder._get(())
      when("runState(" + randomInt + ")")
      val tmp = computation.runState(randomInt)(())
      then("_get(()) -> " + tmp)
      tmp should equal (randomInt, randomInt)
    }


    scenario("_liftT") {
      val computation = builder._liftT (IdentityMonadBuilder._return("X"))
      val tmp = computation.runState(randomInt)(())
      then("_liftT is " + tmp)
      tmp should equal ("X", randomInt)
    }

    scenario("_modify") {
      val computation = builder._modify(_ + 1)
      when("increasing state by one from " + randomInt)
      val tmp = computation.runState(randomInt)(())
      then("the new state is " + tmp)
      tmp should equal (((),randomInt+1))
    }

    scenario("_return") {
      when("computation = _return(" + randomInt + ")")
      val computation = builder._return(randomInt)
      then ("computation is " + computation)
      val tmp = computation.run(randomInt+1)
      then("tmp = computation.run(" + (randomInt + 1) + ") is " + tmp)
      val tmp1 = tmp.run(())
      then("tmp1 = tmp.run(()) " + tmp1)
      tmp1 should equal (randomInt, randomInt + 1)
    }

    scenario("_set") {
      when("_set(" + randomInt + ")")
      val computation = builder._set(randomInt)
      val tmp = computation.runState(randomInt)(())
      then("_set("+randomInt + ") -> " + tmp)
      tmp should equal ((), randomInt)
    }
  }

  feature("Advanced Features") {
    scenario("for-comprehension") {
      val computation =
        for {
          s <- builder._get(())
          x <- builder._modify(_ + 1)
          y <- builder._modify(_ + 2)
        } yield y
      computation.runState(0)(()) should equal ((),3)
    }

    scenario ("take any type as value") {
       // can take any type as the value part
       // only take Int as the State part
       builder._return(1).runState(0)(()) should equal ((1,0))
       builder._return("abc").runState(0)(()) should equal (("abc",0))
       builder._return(List()).runState(0)(()) should equal ((List(),0))
    }
  }

  /*
  feature("Either") {
    scenario("left.isDefine => left") {
      val computation =
        for {
          s <- builder._return(Left("1"))
          _ <- builder._modify(_+1)
          x <- builder._return(Right("2"))
        } yield {}(s, x)
      computation.runState(0)(()) should equal ((),2)
    }
  }
  */
}