package org.scalair.monad


object TraversableMonadBuilder extends MonadBuilderTrait {
  type M[+Z] = TraversableMonad[Z]
  type In[-Z] = Unit
  type Out[+Z] = Traversable[Z]
  override def _return[Z] = z => TraversableMonad(Traversable(z))
  def _close[Z]: Traversable[Z] => M[Z] = TraversableMonad(_)

  case class TraversableMonad[+A](val open: Traversable[A]) extends Monad[A] {
    override def run[B >: A](u: Unit): Traversable[B] = this.open
    override def flatMap[B](a_f_mb: A => M[B]) =
      _close(
        for {
          a <- this.open
          b <- a_f_mb(a).open
        } yield b)
  }
}