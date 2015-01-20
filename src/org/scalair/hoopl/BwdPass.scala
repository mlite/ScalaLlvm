package org.scalair.hoopl
import  TypeDefines._

/**
 * User: wangn
 */

trait BwdPass[N<:NonLocal,F<:Ordered[F]] extends Pass[F] {
  def isFwd = false
  val lattice:DataflowLattice[F]
  val transfer:BwdTransfer[N,F]
  val rewrite:HooplBwdRewrite[N,F]
}

trait BwdTransfer[N<:NonLocal,F<:Ordered[F]] extends HoopMonad {
  def first:Block[N,C,O]=>F=>F
  def middle:Block[N,O,O]=>F=>F
  def last:Block[N,O,C]=>FactBase[F]=>F
}

trait BwdRewrite[N<:NonLocal,F<:Ordered[F]] extends HoopMonad {
  def first:Block[N,C,O]=>F=>CMM#M[Option[Graph[N,C,O]]]
  def middle:Block[N,O,O]=>F=>CMM#M[Option[Graph[N,O,O]]]
  def last:Block[N,O,C]=>FactBase[F]=>CMM#M[Option[Graph[N,O,C]]]
}


trait HooplBwdRewrite[N<:NonLocal,F<:Ordered[F]] extends HoopMonad {
  def first:Block[N,C,O]=>F=>CMM#M[Option[(Graph[N,C,O], HooplBwdRewrite[N,F])]]
  def middle:Block[N,O,O]=>F=>CMM#M[Option[(Graph[N,O,O], HooplBwdRewrite[N,F])]]
  def last:Block[N,O,C]=>FactBase[F]=>CMM#M[Option[(Graph[N,O,C], HooplBwdRewrite[N,F])]]

  def noRewrite[E <:Status,X<:Status]:(N=>F=>CMM#M[Option[(Graph[N,E,X], HooplBwdRewrite[N,F])]]) = { n => f =>
    assert(cmObj != null, "CMMObj is null")
    cmObj._return[Option[(Graph[N,E,X], HooplBwdRewrite[N,F])]](None)
  }
}

object HooplBwdRewrite extends HoopMonad {
  type FT[e<:Status,x<:Status,n<:NonLocal,f<:Ordered[f]] = Block[n,e,x]=>f=>CMM#M[Option[Graph[n,e,x]]]

  def mkRewrite3[N<:NonLocal,F<:Ordered[F]](bwRw:BwdRewrite[N,F]):HooplBwdRewrite[N,F] = {
    def asRew[t]:t => (t, HooplBwdRewrite[N,F]) = { g:t => (g, mkRewrite3[N,F](new BwdNoRewrite[N,F])) }

    def withfuel[A]:Option[A]=>CMM#M[Option[A]] = { x:Option[A] =>
      cmObj.withFuel[A](x).asInstanceOf[cmObj.M[Option[A]]]
    }

    def lift[t,t1,A]:(t=>t1=>CMM#M[Option[A]])=>t=>t1=>CMM#M[Option[(A, HooplBwdRewrite[N,F])]] = {
      rw:(t=>t1=>CMM#M[Option[A]])=>node:t=>fact:t1 =>
        val m:CMM#M[Option[A]] = rw(node)(fact).flatMap[Option[A]](withfuel[A])

        def lf1[a]:Option[a] => Option[(a, HooplBwdRewrite[N,F])] = { x:Option[a] =>
          x match {
            case Some(t1) => Some(asRew[a](t1))
            case None => None
          }
        }
        val lf2:CMM#M[Option[A]]=> CMM#M[Option[(A, HooplBwdRewrite[N,F])]] =
          cmObj._liftM[Option[A],Option[(A, HooplBwdRewrite[N,F])]](lf1[A])
        lf2(m)
    }
    new HooplBwdRewrite[N,F] {
      def first = lift[Block[N,C,O], F, Graph[N,C,O]](bwRw.first)
      def middle = lift[Block[N,O,O], F, Graph[N,O,O]](bwRw.middle)
      def last = lift[Block[N,O,C], FactBase[F], Graph[N,O,C]](bwRw.last)
    }
  }
}

trait BwdSingleRewrite[N<:NonLocal,F<:Ordered[F]] extends BwdRewrite[N,F] {
  def rewrite[E<:Status,X<:Status]:N=>Fact[X,F]=>CMM#M[Option[Graph[N,E,X]]]

  final def first:(Block[N,C,O]=>F=>CMM#M[Option[Graph[N,C,O]]]) = { block => f =>
    rewrite[C,O](block.node)(Left(f))
  }
  final def middle:(Block[N,O,O]=>F=>CMM#M[Option[Graph[N,O,O]]]) = { block => f =>
    rewrite[O,O](block.node)(Left(f))
  }
  final def last:(Block[N,O,C]=>FactBase[F]=>CMM#M[Option[Graph[N,O,C]]]) = { block => f =>
    rewrite[O,C](block.node)(Right(f))
  }
}

class BwdNoRewrite[N<:NonLocal,F<:Ordered[F]] extends BwdSingleRewrite[N,F] {
  def rewrite[E <:Status,X<:Status]:N=>Fact[X,F]=>CMM#M[Option[Graph[N,E,X]]] = { n => f =>
    assert(cmObj != null, "CMMObj is null")
    cmObj._return[Option[Graph[N,E,X]]](None)
  }
}