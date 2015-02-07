package org.scalair.ir.rewriter

import org.scalair.ScalairFeatureSpecTrait
import org.scalair.ir.imir.{LocalID, LocalVar}

/**
 * User: wangn
 * Date: 10/17/11
 */

class ImIrVarOrIdRewriterTraitSpec extends ScalairFeatureSpecTrait {
  val rewriter = new ImIrVarOrIdRewriter[Set[Int],Int] {
    override def clLocalID(env:Int)(x:A[LocalID]):A[LocalID] = x flatMap {
      _ match {
        case Left(x0) => builder._closeT { s:Set[Int] => builder._fromReturn((Right(x0), s + x0.n)) }
        case _ => x
      }
    }

    def combine[A,B](x:Either[A,A], y:Either[B,B]):Either[(A,B),(A,B)] = {
      (x,y) match {
        case (Left(x1), Left(x2)) => Left((x1,x2))
        case (Left(x1), Right(x2)) => Right((x1,x2))
        case (Right(x1), Left(x2)) => Right((x1,x2))
        case (Right(x1), Right(x2)) => Right((x1,x2))
      }
    }
  }
  val emptySet = Set[Int]()

  feature("Test StateToptionMonad") {
    scenario("Left,Left => Left") {
      val computation =
        for {
          x <- rewriter.clVarOrID(10)(rewriter.unit(LocalVar("a")))
          y <- rewriter.clVarOrID(20)(rewriter.unit(LocalVar("b")))
        } yield rewriter.combine(x,y)
      computation.runState(emptySet)(()) must equal (Left(LocalVar("a"),LocalVar("b")), emptySet)
    }


    scenario("Right,Left => Right") {
      val computation =
        for {
          x <- rewriter.clVarOrID(10)(rewriter.unit(LocalID(1)))
          y <- rewriter.clVarOrID(20)(rewriter.unit(LocalVar("b")))
        } yield rewriter.combine(x,y)
      computation.runState(emptySet)(()) must equal (Right(LocalID(1),LocalVar("b")), Set(1))
    }

    scenario("Right,Right => Right") {
      val computation =
        for {
          x <- rewriter.clVarOrID(10)(rewriter.unit(LocalID(1)))
          y <- rewriter.clVarOrID(20)(rewriter.unit(LocalID(2)))
        } yield rewriter.combine(x,y)
      computation.runState(emptySet)(()) must equal (Right(LocalID(1),LocalID(2)), Set(1,2))
    }
  }
}