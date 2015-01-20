package org.scalair.monad

/*
object TraversableChoiceMonadObject  extends ChoiceMonadBuilderTrait {
  type M[+Z] = TraversableChoiceMonad[Z]
  type In[-Z] = Unit
  type Out[+Z] = Traversable[Z]
  override def _return[Z] = z => TraversableChoiceMonad(Traversable(z))
  override def _nothing[Z] = new TraversableChoiceMonad(Traversable())
  def _close[Z]: Traversable[Z] => M[Z] = TraversableChoiceMonad(_)

  case class TraversableChoiceMonad[+A](val open: Traversable[A]) extends ChoiceMonad[A] {
    override def run[B >: A](u: Unit): Traversable[B] = this.open
    override def flatMap[B](a_f_mb: A => M[B]): M[B] =
      _close(for {
        a <- this.open
        b <- a_f_mb(a).open
      } yield b)
    override def or[B >: A](mb: => M[B]) =
      _close(this.open ++ mb.open)
  }
}
*/