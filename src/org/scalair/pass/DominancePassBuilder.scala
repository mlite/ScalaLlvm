package org.scalair.pass

/**
 * User: wangn
 * Date: 7/23/11
 */

import org.scalair.hoopl._
import org.scalair.hoopl.TypeDefines._

abstract class DominancePassBuilder[N<:NonLocal]() extends PassFwdBuilder[N] { self =>
/*
 * Represents part of the dominance relation: each label
 * in a list is dominated by all its successors *in the list*.
 *
 * Example:
 *
 *      A
 *     / \
 *    B  C
 *     \/
 *     D
 *
 *  entry:
 *  A ->DPath[]
 *
 *  Init:
 *  A -> DPath[A]
 *  B -> DPath[B]
 *  C -> DPath[C]
 *  D -> DPath[D]
 *
 *  A -> DPath[A]
 *  B -> DPath[B,A]
 *  C -> DPath[C,A]
 *  D -> DPath[D,A]
 *
 */
  case class DPath(val path:List[BlockId]) extends Ordered[DPath] {
    def compare(that:DPath) = compareListInt(this.path, that.path)
  }
  type F = Pointed[DPath]

  val lattice = new DataflowLattice[F] {
    val name = "DPath"
    val bot = Pointed.getbot[DPath]()

    private val joinDPath:JoinFun[DPath] = new JoinFun[DPath] {
      def apply(id:BlockId, oldFact:OldFact[DPath], newFact:NewFact[DPath]):(ChangeFlag, DPath) = {
        logger.debug("[join] is called {}", id)
        def dropUnlink[A](l:List[A], l1:List[A], maybe_like:List[A]):List[A] = {
          (l, l1, maybe_like) match {
            case (List(), List(), maybe_like) => maybe_like
            case (x::xs, y::ys, maybe_like) => dropUnlink[A](xs, ys, if (x==y) maybe_like else xs)
            case (_,_,_) => throw  new Exception()
        }
        }
        // longest common *suffix*
        def lcs(l:List[BlockId], l1:List[BlockId]):List[BlockId] = {
          if (l.length > l1.length) lcs (l.drop(l.length - l1.length), l1)
          else if (l.length < l1.length) lcs (l1, l)
          else dropUnlink[BlockId](l,l1,l)
      }
        def lengthDiffs[A](l:List[A], l1:List[A]):Boolean = {
          (l, l1) match {
            case (List(), List()) => false
            case (_::xs, _::ys) => lengthDiffs(xs, ys)
            case (List(), _::_) => true
            case (_::_, List()) => true
        }
        }
        val l = oldFact.x.path
        val l1 = newFact.x.path
        val j = lcs(l, l1)
        (ChangeFlag.changeIf(lengthDiffs(l,j)), DPath(j))
      }
    }
    val join = joinDPath.liftToJoinWithTop
  }


  /*
   * The fact that goes into the entry of a dominator analysis: the first node
   * is dominated only by the entry point, which is represented by the empty list
   * of blockIds
   */
  val entry:F = Pointed.pelem(DPath(List()))

  // 2 define transfer functions
  val transfer = new FwdTransfer[N,F] {
    def first = { b:Block[N,C,O] => f:F =>
      def extendDom:BlockId=>DPath =>DPath = { id => dpath => DPath(id::dpath.path) }
      val x = f.map(extendDom(b.blockId))
      logger.debug("[first] {}", x)
      x
    }
    def middle = { b:Block[N,O,O] => f:F => f }
    def last = { b:Block[N,O,C] => f:F  => b.distributeFact[F](f) }
  }

  // 3 define rewrite functions
  val rewrite = new FwdNoRewrite[N,F] {}


  type Image = DominatorTree
  def toImage(fb:FactBase[F]):Image =  {
    val fb1 = fb// + (1 -> entry)
    DominatorTree.tree(fb1.toList.map { case (k,v) => (k, v.getOrElse(DPath(List())).path) })
  }
}