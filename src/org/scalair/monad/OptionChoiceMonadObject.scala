package org.scalair.monad


/*
object OptionChoiceMonadObject extends ChoiceMonadBuilderTrait {
  type M[+Z] = OptionChoiceMonad[Z]
  type In[-Z] = Unit
  type Out[+Z] = Option[Z]
  override def _return[Z] = z => OptionChoiceMonad(Some(z))
  override def _nothing[Z] = OptionChoiceMonad(None)
  def _close[Z]: Option[Z] => M[Z] = OptionChoiceMonad(_)
  implicit def _ocm_f_tcm[Z]: M[Z] => TraversableChoiceMonadObject.M[Z] =
    mz => TraversableChoiceMonadObject._close(mz.open)

  case class OptionChoiceMonad[+A](val open: Option[A]) extends ChoiceMonad[A] {
    override def run[B >: A](u: Unit): Option[B] = this.open
    override def flatMap[B](a_f_mb: A => M[B]): M[B] =
      _close(for {
        a <- this.open
        b <- a_f_mb(a).open
      } yield b)
    override def or[B >: A](mb: => M[B]) =
      _close(this.open orElse mb.open)
  }
}
*/