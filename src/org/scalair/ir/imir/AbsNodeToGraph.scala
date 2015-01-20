package org.scalair.ir.imir

import org.scalair.hoopl.{GraphBuilder, Status, Graph}

/**
 * User: wangn
 * Date: 8/21/11
 */

trait AbsNodeToGraph extends GraphBuilder[ET[AbsNode]] {
  import ET._
  def toG[E<:Status,X<:Status](node:ET[AbsNode]):Graph[ET[AbsNode],E,X] = asT(node) match {
    case FirstNode(label, entry) => mkFirst(node).castEX[E,X]
    case StartNode(label, entry) => mkMiddle(node).castEX[E,X]
    case MiddleNode(inst) => mkMiddle(node).castEX[E,X]
    case LastNode(targets, ctrl) => mkLast(node).castEX[E,X]
  }
}