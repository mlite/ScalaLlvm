package org.scalair.ir.rewriter

import org.scalair.hoopl.{Graph, TypeDefines, FwdSingleRewrite, Status}
import TypeDefines._
import org.scalair.ir.imir.{AbsNodeToGraph, AbsNode,ET}

/**
 * User: wangn
 * Date: 8/21/11
 */
trait ImIrFwdRewrite[F<:Ordered[F]] extends FwdSingleRewrite[ET[AbsNode],F] with AbsNodeToGraph {
  def rewriteAbsNode(f:F)(n:ET[AbsNode]):Option[ET[AbsNode]]

  final def rewrite[E<:Status,X<:Status]:ET[AbsNode]=>F=>CMM#M[Option[Graph[ET[AbsNode],E,X]]] = {
    oldNode => fact =>
      val newNode = rewriteAbsNode(fact)(oldNode)
      val g = newNode.map(toG[E,X])
      cmObj._return[Option[Graph[ET[AbsNode],E,X]]](g)
  }
}