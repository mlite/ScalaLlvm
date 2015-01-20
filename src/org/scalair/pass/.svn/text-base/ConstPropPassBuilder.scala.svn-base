package org.scalair.pass

import org.scalair.hoopl._
import org.scalair.hoopl.TypeDefines._

/**
 * User: wangn
 * Date: 8/6/11
 */

abstract class ConstPropPassBuilder[N <:NonLocal, V<:Ordered[V], C<:Ordered[C]] extends PassFwdBuilder[N] { self =>
  type F = OrderedMap[V,Pointed[C]]

  final val lattice = new DataflowLattice[F] {
    val name = "constant"

    val bot = OrderedMap[V,Pointed[C]](Map[V,Pointed[C]]())

    private val joinConst:JoinFun[Pointed[C]] = new JoinFun[Pointed[C]] {
      def apply(id:BlockId, oldFact:OldFact[Pointed[C]], newFact:NewFact[Pointed[C]]):(ChangeFlag, Pointed[C]) = {
        if (oldFact.x.compare(newFact.x) == 0) (NoChange, oldFact.x)
        else (SomeChange, Pointed.gettop[C]())
      }
    }
    val join:JoinFun[F] = joinConst.liftToJoinMaps[V]
  }

  val entry:F = lattice.bot

  // 3 define rewrite functions

  type Image = FactBase[F]
  def toImage(fb:FactBase[F]) = fb
}