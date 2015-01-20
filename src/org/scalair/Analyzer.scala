package org.scalair

import hoopl.TypeDefines._
import hoopl.{FwdNoRewrite, Pointed, NonLocal, AnalyzerAndRewriterFwdBuilder}
import ir.imir._
import ir.transfer.ImIrInstTransfer
import pass.{ConstPropPassBuilder, DominancePassBuilder}

/**
 * User: wangn
 * Date: 7/30/11
 */

object Analyzer {
  /*
  def getDominaceAnalyzer[N<:NonLocal]() = {
    new FwdAnalyzerAndRewriterBuilder[N] {
      val passBuilder = new DominancePassBuilder[N]() {
      }
    }
  }

  def getConstPopAnalyzer() = {
    new FwdAnalyzerAndRewriterBuilder[AbsNode] {
      val passBuilder = new ConstPropPassBuilder[AbsNode, VarOrID, Const] () {
        override type F = OrderedMap[VarOrID, Pointed[Const]]
        val transfer = new ImIrInstTransfer[F] {
          val vis = new InstVisitor[F] {
            override def bcInstruction(fact: F)(x: AbsInst): F = x match {
              case Comment(_) => fact
              case Instruction(lhs, rhs, _) => {
                if (lhs.isDefined && rhs.isInstanceOf[Const])
                  fact + (lhs.get -> Pointed.pelem(rhs.asInstanceOf[Const]))
                else
                  fact
              }
            }

            override def bcRHS(fact:F)(x:RHS):F = x match {
              case Store(_,v:Const, ptr:VarOrID,_) => {
                fact + (ptr -> Pointed.pelem(v))
              }
              case _ => fact
            }
          }
        }
        val rewrite = new FwdNoRewrite[AbsNode,F] {}
      }
    }
  }
  */
}