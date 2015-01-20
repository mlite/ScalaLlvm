package org.scalair.hoopl

import TypeDefines._

/**
 * User: wangn
 */

trait FwdPass[N<:NonLocal,F<:Ordered[F]] extends Pass[F] {
  def isFwd = true
  val lattice:DataflowLattice[F]
  val transfer:FwdTransfer[N,F]
  val rewrite:HooplFwdRewrite[N,F]
}


trait FwdTransfer[N<:NonLocal,F<:Ordered[F]] extends HoopMonad {
  def first:Block[N,C,O]=>F=>F
  def middle:Block[N,O,O]=>F=>F
  def last:Block[N,O,C]=>F=>FactBase[F]
}



trait FwdRewrite[N<:NonLocal,F<:Ordered[F]] extends HoopMonad {
  def first:Block[N,C,O]=>F=>CMM#M[Option[Graph[N,C,O]]]
  def middle:Block[N,O,O]=>F=>CMM#M[Option[Graph[N,O,O]]]
  def last:Block[N,O,C]=>F=>CMM#M[Option[Graph[N,O,C]]]
}


trait HooplFwdRewrite[N<:NonLocal,F<:Ordered[F]] extends HoopMonad {
  def first:Block[N,C,O]=>F=>CMM#M[Option[(Graph[N,C,O], HooplFwdRewrite[N,F])]]
  def middle:Block[N,O,O]=>F=>CMM#M[Option[(Graph[N,O,O], HooplFwdRewrite[N,F])]]
  def last:Block[N,O,C]=>F=>CMM#M[Option[(Graph[N,O,C], HooplFwdRewrite[N,F])]]

  def noRewrite[E <:Status,X<:Status]:(N=>F=>CMM#M[Option[(Graph[N,E,X], HooplFwdRewrite[N,F])]]) = { n => f =>
    assert(cmObj != null, "CMMObj is null")
    cmObj._return[Option[(Graph[N,E,X], HooplFwdRewrite[N,F])]](None)
  }
}

object HooplFwdRewrite extends HoopMonad {
  type FT[e<:Status,x<:Status,n<:NonLocal,f<:Ordered[f]] = Block[n,e,x]=>f=>CMM#M[Option[Graph[n,e,x]]]

  def mkRewrite3[N<:NonLocal,F<:Ordered[F]](fwRw:FwdRewrite[N,F]):HooplFwdRewrite[N,F] = {
    def asRew[t]:t => (t, HooplFwdRewrite[N,F]) = { g:t => (g, mkRewrite3[N,F](new FwdNoRewrite[N,F])) }

    def withfuel[A]:Option[A]=>CMM#M[Option[A]] = { x:Option[A] =>
      cmObj.withFuel[A](x).asInstanceOf[cmObj.M[Option[A]]]
    }

    def lift[t,t1,A]:(t=>t1=>CMM#M[Option[A]])=>t=>t1=>CMM#M[Option[(A, HooplFwdRewrite[N,F])]] = {
      rw:(t=>t1=>CMM#M[Option[A]])=>node:t=>fact:t1 =>
        val m:CMM#M[Option[A]] = rw(node)(fact).flatMap[Option[A]](withfuel[A])

        def lf1[a]:Option[a] => Option[(a, HooplFwdRewrite[N,F])] = { x:Option[a] =>
          x match {
            case Some(t1) => Some(asRew[a](t1))
            case None => None
          }
        }
        val lf2:CMM#M[Option[A]]=> CMM#M[Option[(A, HooplFwdRewrite[N,F])]] =
          cmObj._liftM[Option[A],Option[(A, HooplFwdRewrite[N,F])]](lf1[A])
        lf2(m)
    }
    new HooplFwdRewrite[N,F] {
      def first = lift[Block[N,C,O], F, Graph[N,C,O]](fwRw.first)
      def middle = lift[Block[N,O,O], F, Graph[N,O,O]](fwRw.middle)
      def last = lift[Block[N,O,C], F, Graph[N,O,C]](fwRw.last)
    }
  }
}

trait FwdSingleRewrite[N<:NonLocal,F<:Ordered[F]] extends FwdRewrite[N,F] {
  def rewrite[E<:Status,X<:Status]:N=>F=>CMM#M[Option[Graph[N,E,X]]]

  final def first:(Block[N,C,O]=>F=>CMM#M[Option[Graph[N,C,O]]]) = { block => f =>
    rewrite[C,O](block.node)(f)
  }
  final def middle:(Block[N,O,O]=>F=>CMM#M[Option[Graph[N,O,O]]]) = { block => f =>
    rewrite[O,O](block.node)(f)
  }
  final def last:(Block[N,O,C]=>F=>CMM#M[Option[Graph[N,O,C]]]) = { block => f =>
    rewrite[O,C](block.node)(f)
  }
}


class FwdNoRewrite[N<:NonLocal,F<:Ordered[F]] extends FwdSingleRewrite[N,F] {
  def rewrite[E <:Status,X<:Status]:(N=>F=>CMM#M[Option[Graph[N,E,X]]]) = { n => f =>
    assert(cmObj != null, "CMMObj is null")
    cmObj._return[Option[Graph[N,E,X]]](None)
  }
}