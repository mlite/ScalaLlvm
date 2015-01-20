package org.scalair.ir

import org.scalair.util.Logging
import org.scalair.hoopl.AnalyzerAndRewriterFwdBuilder
import org.scalair.ir.imir._
import org.scalair.ir.transfer.ImIrTopLevelTransfer

/**
 * User: wangn
 * Date: 9/15/11
 */

trait ModulePass extends Logging { self =>
  import ET._
  val codeFwdAnalysisAndOptimization:AnalyzerAndRewriterFwdBuilder[ET[AbsNode]]
  type F = codeFwdAnalysisAndOptimization.F

  val topLevelTransfer: ImIrTopLevelTransfer[F]

  def rewriteFunctionDef(fact:F)(x:FunctionDef) = {
    val g0 = x.graph.mapInst { v => ET(x.env, v) }
    val (g1, dt) = codeFwdAnalysisAndOptimization.run(g0)
    val g2 = g1.mapInst { v  => asT(v) }
    (FunctionDef(x.env, x.funProto, g2), dt)
  }

  def rewriteModule(fact:F)(m:Module) = {
    // two iteration is sufficient to collect the fixed-point of toplevel properties.
    val factx = m.list.foldLeft(fact) { (p, tp) => topLevelTransfer.clTopLevel(p)(ET(m.defs, tp)) }
    val facty = m.list.foldLeft(factx) { (p, tp) => topLevelTransfer.clTopLevel(p)(ET(m.defs, tp)) }
    val (nlist, results) = m.list.foldLeft((List[TopLevel](), Map[GlobalVarOrID, codeFwdAnalysisAndOptimization.passBuilder.Image]())) { (p, tp) =>
      if (tp.isInstanceOf[FunctionDef]) {
        val nf = rewriteFunctionDef(facty)(tp.asInstanceOf[FunctionDef])
        (nf._1::p._1, p._2  + (nf._1.funProto.name -> nf._2))
      } else {
        (tp::p._1, p._2)
      }
    }
    logger.info("{}", results)
    (Module(nlist, m.defs), results)
  }

  def run(m:Module) = rewriteModule(codeFwdAnalysisAndOptimization.passBuilder.entry)(m)
}