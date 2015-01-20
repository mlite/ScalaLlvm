package org.scalair.ir

import common.Add
import imir._
import org.scalair.hoopl.{Pointed, AnalyzerAndRewriterFwdBuilder}
import org.scalair.hoopl.TypeDefines.OrderedMap
import org.scalair.pass.ConstPropPassBuilder
import rewriter.ImIrFwdRewrite
import transfer._

/**
 * User: wangn
 * Date: 10/8/11
 */

object ModuleConstPropPass extends ModulePass { self =>
  import  ET._

  val codeFwdAnalysisAndOptimization = new AnalyzerAndRewriterFwdBuilder[ET[AbsNode]] {
    val passBuilder = new ConstPropPassBuilder[ET[AbsNode], VarOrID, Const] () {
      val transfer = new MyInstTransfer {}
      val rewrite = new ImIrFwdRewrite[F] {
        def rwLoadSrc(f:F)(v:Value):Option[Const] = v match {
          case x:VarOrID => f.get(x) match {
            case Some(y) => y.getOrNone
            case None => None
          }
          case _ => None
        }
        def rwRHS(f:F)(rhs:ET[RHS]):Option[RHS] = asT(rhs) match {
          case Load(vol, ptr, align) => rwLoadSrc(f)(ptr).map { ConstExpr (_) }
          case x:ArithmaticExpr => ConstantFolder.foldValue(f)(ET(rhs.env,x)).map { ConstExpr(_) }
          case _ => None
        }
        def rwInstruction(f:F)(x:ET[AbsInst]):Option[AbsInst] = asT(x) match {
          case Comment(_) => None
          case Instruction(lhs, rhs, dbg) => {
            val rhs1 = rwRHS(f)(asA(x,rhs))
            rhs1.map { Instruction(lhs, _, dbg) }
          }
        }
        def rewriteAbsNode(f:F)(x:ET[AbsNode]):Option[ET[AbsNode]] = asT(x) match {
          case FirstNode(l, e) => None
          case MiddleNode(n) => rwInstruction(f)(asA(x,n)).map { v => asA(x,MiddleNode(v)) }
          case LastNode(targets, n) => None
          case _ => throw new Exception()
        }
      }
    }
  }

  type F1 = OrderedMap[VarOrID, Pointed[Const]]

  trait MyTypeAndConstTransfer extends ImIrTypeAndConstTransfer[F1] {
    trait ThisTypeAndConstVisitor extends TypeAndConstVisitor[F1] {
    }
  }

  trait MyInstTransfer extends ImIrInstTransferFwd[F1] with MyTypeAndConstTransfer {
    val vis = new MyInstVisitor {}

    trait MyInstVisitor extends InstVisitor[F1] with ThisTypeAndConstVisitor {
      def rhsConstFolder(x:RHS):Option[Const] = x match {
        case ConstExpr(c) => Some(c)
        case _ => None
      }
      override def bcInstruction(fact: F1)(x: ET[AbsInst]): F1 = asT(x) match {
        case Comment(_) => fact
        case Instruction(lhs, rhs, _) => {
          val r = for {
            lhs0 <- lhs;
            rhs0 <- rhsConstFolder(rhs)
          } yield (fact + (lhs0 -> Pointed.pelem(rhs0)))
          r.getOrElse(fact)
        }
      }

      def storeSrcConstFolder(fact:F1)(x:ET[Value]):Option[Const] = asT(x) match {
        case c:Const => Some(c)
        case i:VarOrID => fact.get(i) match {
          case Some(i0) => i0.getOrNone
          case None => None
        }
      }

      override def bcRHS(fact:F1)(x:ET[RHS]):F1 = asT(x) match {
        case Store(_,v, ptr:VarOrID,_) => {
          val r =
            for { src <- storeSrcConstFolder(fact)(asA(x,v)) }
            yield (fact + (ptr -> Pointed.pelem(src)))
          r.getOrElse(fact)
        }
        case _ => fact
      }
    }
  }


  val topLevelTransfer: ImIrTopLevelTransfer[F1] = new ImIrTopLevelTransfer[F1] with MyTypeAndConstTransfer {
    val vis = new TopLevelVisitor[F1] with ThisTypeAndConstVisitor {
    }
  }
}