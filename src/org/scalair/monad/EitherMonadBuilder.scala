package org.scalair.monad

import org.scalair.monad.OptionMonadBuilder.OptionMonad._

/*
object EitherMonadBuilder extends MonadBuilderTrait {
  type M[+Z:Either[_,_]] = EitherMonad[Z]
  type In[-Z] = Unit
  type Out[+Z] = Z
  override def _return[Z] = z => EitherMonad(z)

  def _close[A,B]: Either[A,B] => M[Either[A,B]] = EitherMonad(_)
  /*
  implicit def _om_f_tm[Z]: M[Z] => TraversableMonadBuilder.M[Z] =
    mz => TraversableMonadBuilder._close(mz.open)
    */

  case class EitherMonad[+A](val open: A) extends Monad[A] {
    override def run[A1>:A](u: Unit): A1 = this.open

    override def flatMap[B](a_f_mb: A => M[B]): M[B] =
      _close(for {
        a <- this.open
        b <- a_f_mb(a).open
      } yield b)
  }
}
*/