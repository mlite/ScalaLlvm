package org.scalair.monad

object OptionMonadBuilder extends MonadBuilderTrait {
  type M[+Z] = OptionMonad[Z]
  type In[-Z] = Unit
  type Out[+Z] = Option[Z]
  override def _return[Z] = z => OptionMonad(Some(z))

  def _close[Z]: Option[Z] => M[Z] = OptionMonad(_)
  /*
  implicit def _om_f_tm[Z]: M[Z] => TraversableMonadBuilder.M[Z] =
    mz => TraversableMonadBuilder._close(mz.open)
    */

  case class OptionMonad[+A](val open: Option[A]) extends Monad[A] {
    override def run[B >: A](u: Unit): Option[B] = this.open

    override def flatMap[B](a_f_mb: A => M[B]): M[B] =
      _close(for {
        a <- this.open
        b <- a_f_mb(a).open
      } yield b)
  }
}