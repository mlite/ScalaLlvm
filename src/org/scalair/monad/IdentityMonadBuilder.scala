package org.scalair.monad

/**
 * User: wangn
 * Date: 9/24/11
 */
object IdentityMonadBuilder extends MonadBuilderTrait {
  type M[+Z] = IdentityMonad[Z]
  type In[-Z] = Unit
  type Out[+Z] = Z

  override def _return[Z]:Z => IdentityMonad[Z] = { x:Z =>
    new IdentityMonad[Z] { val open = x }
  }

  trait IdentityMonad[+A] extends Monad[A] {
    val open:A
    // just expose this value
    override def run[B >: A](u:Unit):B = open

    override def flatMap[B](a_f_mb:A=>M[B]) = a_f_mb(open)

    override def toString = "IdentityMonad[" + open + "]"
  }
}