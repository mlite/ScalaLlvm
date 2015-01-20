package org.scalair.pass

import org.scalair.hoopl.TypeDefines._
import collection.immutable.List._

/**
 * User: wangn
 * Date: 8/6/11
 */

sealed abstract class DominatorNode () {
  def dotnode:String
}
final case object Entry extends DominatorNode {
  def dotnode = "  entryNode [shape=plaintext, label=\"entry\"]\n"
  override def toString = "entryNode"
}
final case class Labelled(id:BlockId) extends DominatorNode {
  def dotnode = "  " + id + "\n"
  override def toString = id.toString
}


/*
 *  This data structure is a rose tree in which each node may have arbitrary
 *  many children. Each node dominates all its descendants.
 */
case class DominatorTree(val root:DominatorNode, val children:List[DominatorTree]) {
  def toDot:String = {
    def outedges = children.foldLeft("") { (p, e) =>
      p + "  " + root + " -> " + e.root + "\n "
    }
    root.dotnode + outedges + children.foldLeft("") { (p, e) =>
      p + e.toDot
    }
  }
}

object DominatorTree {
  /*
   * list: BlockId dominates Dominace
   */
  def tree(facts:List[(BlockId, List[BlockId])]):DominatorTree = {
    def merge(l:List[List[BlockId]]):List[DominatorTree] = mapTree(children(l.filter(!_.isEmpty)))
    def addList(map:FactBase[List[List[BlockId]]], lst:List[BlockId]):FactBase[List[List[BlockId]]] = {
      lst match {
        case x::xs => {
          val existing = map.get(x).getOrElse(List())
          map + ((x, xs::existing))
        }
        case Nil => throw new Exception("this cannot happen")
      }
    }
    def children(l:List[List[BlockId]]):FactBase[List[List[BlockId]]] =
      l.foldLeft(noFacts[List[List[BlockId]]])(addList)

    def mkList:((BlockId, List[BlockId])) => List[BlockId] = { case (l, doms) => l::doms }
    def mapTree(factBase:FactBase[List[List[BlockId]]]):List[DominatorTree] = {
      for ((x,ls) <-factBase.toList) yield DominatorTree(Labelled(x), merge(ls))
    }
    val listOfList:List[List[BlockId]] = facts.map(mkList)
    DominatorTree(Entry, merge(listOfList.map(_.reverse)))
  }

  def tree2dot(d:DominatorTree):String = {
    "digraph abc {\n" + d.toDot  + "}\n"
  }
}