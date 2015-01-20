package org.scalair.monad

trait TransformedMonadBuilderTrait extends MonadBuilderTrait {
  type MonadBuilderType <: MonadBuilderTrait
  implicit val monadBuilder: MonadBuilderType
  type FromM[+Z] = monadBuilder.M[Z]
  type FromIn[Z] = monadBuilder.In[Z]
  type FromOut[Z] = monadBuilder.Out[Z]

  def _fromReturn[Z]: Z => FromM[Z] = monadBuilder._return(_)

  type M[+Z] <: TransformedMonad[Z]
  type ToM[+Z]
  def _closeT[Z]: ToM[Z] => M[Z]
  def _lift[Z]: FromM[Z] => ToM[Z]
  def _liftT[Z]: FromM[Z] => M[Z] = _lift[Z] andThen _closeT[Z]
  override def _return[Z] =
    z =>
      _liftT {
        _fromReturn(z)
      }

  trait TransformedMonad[+A] extends Monad[A] { self: M[A] =>
    def openT: ToM[A]
  }
}