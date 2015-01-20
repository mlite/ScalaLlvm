package org.scalair.monad

trait MonadBuilderTrait extends ComputationBuilderTrait {
  type M[+Z] <: Monad[Z]
  def _return[Z]: Z => M[Z]
  def _join[Z]: M[M[Z]] => M[Z] = _.flatMap(identity)
  def _liftM[A,R](f:(A => R)):(M[A] => M[R])  = { ma:M[A] =>
    ma flatMap { a:A => _return[R](f(a)) }
  }

  def _liftM2[A1,A2,R](f:(A1=>A2=>R)):(M[A1]=>M[A2]=>M[R]) = { ma1:M[A1] => ma2:M[A2] =>
    ma1 flatMap { a1:A1 => ma2 flatMap { a2:A2 => _return[R](f(a1)(a2)) } }
  }

  trait Monad[+A] extends Computation[A] { self: M[A] =>
    def flatMap[B](a_f_mb: A => M[B]): M[B]
    def >=[B](a_f_mb: A => M[B]): M[B] = flatMap(a_f_mb)
    def >>[B](mb: => M[B]): M[B] = this flatMap { _ => mb }
    def map[B](afb: A => B) = this flatMap (afb andThen _return)
  }
}