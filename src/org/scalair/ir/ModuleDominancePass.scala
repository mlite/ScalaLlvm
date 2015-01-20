package org.scalair.ir

import imir.{ET, AbsNode}
import org.scalair.pass.DominancePassBuilder
import org.scalair.hoopl.{AnalyzerAndRewriterFwdBuilder}
import transfer.{ImIrTopLevelTransfer}

/**
 * User: wangn
 * Date: 10/8/11
 */

object ModuleDominancePass extends ModulePass {
  val codeFwdAnalysisAndOptimization = new AnalyzerAndRewriterFwdBuilder[ET[AbsNode]] {
    val passBuilder = new DominancePassBuilder[ET[AbsNode]]() {
    }
  }
  val topLevelTransfer = new ImIrTopLevelTransfer[F] {
    val vis = new TopLevelVisitor[F] {
    }
  }
}