package org.scalair.hoopl

import TypeDefines.{CMM}
/**
 * User: wangn
 * Date: 5/29/11
 */

trait GraphBuilder[N <: NonLocal] extends HoopMonad {
  def mkFirst(n:N):Graph[N,C,O] = mkExit(BFirst[N](n))
  def mkMiddle(n:N):Graph[N,O,O] = GUnit[N](BMiddle[N](n))
  def mkLast(n:N):Graph[N,O,C] = mkEntry(BLast[N](n))

  def splice[E<:Status,X<:Status,X1<:Status](g1:Graph[N,E,X], g2:Graph[N,X,X1]):Graph[N,E,X1] =
    g1.splice[X1](g2)

  def catGraphs(l:List[Graph[N,O,O]]):Graph[N,O,O] =
    l.foldLeft(Graph.emptyGraph[N]()) ((p, e) => p.splice[O](e))

  //def mkLabel(id:LabelId):First[N] = mkFirst(mkLabelNode(id))
  //def mkBranch(target:LabelId):Last[N] = mkLast(mkBranchNode(target))
  def mkMiddles(ms:List[N]):Graph[N,O,O] = catGraphs(ms.map(mkMiddle(_)))

  def mkExit(block:Block[N,C,O]):Graph[N,C,O] =
    GMany[N,C,O](Graph.nothingOentry[N](), Graph.emptyBody[N], JustO(block))

  def mkEntry(block:Block[N,O,C]):Graph[N,O,C] =
    GMany[N,O,C](JustO(block), Graph.emptyBody[N], Graph.nothingOexit[N]())

  //def mkLabelNode(l:LabelId):N
  //def mkBranchNode(l:LabelId):N
}