package org.scalair.monad

trait TransformedChoiceMonadBuilderTrait extends TransformedMonadBuilderTrait with ChoiceMonadBuilderTrait {
  override type MonadBuilderType <: ChoiceMonadBuilderTrait
  def _fromNothing[Z]: FromM[Z] = monadBuilder._nothing
  override def _nothing[Z] =
    _liftT {
      _fromNothing
    }
  type M[+Z] <: TransformedChoiceMonadTrait[Z]
  trait TransformedChoiceMonadTrait[+A] extends TransformedMonad[A] with ChoiceMonad[A] { self: M[A] =>
  }
}
