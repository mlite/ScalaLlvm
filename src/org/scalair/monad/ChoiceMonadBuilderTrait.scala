package org.scalair.monad

trait ChoiceMonadBuilderTrait extends MonadBuilderTrait with ChoiceBuilderTrait {
  type M[+Z] <: ChoiceMonad[Z]
  trait ChoiceMonad[+A] extends Monad[A] with Choice[A] { self: M[A] =>
    def withFilter(ap: A => Boolean) =
      flatMap { a =>
        if (ap(a)) {
          _return(a)
        } else {
          _nothing
        }
      }
  }
}