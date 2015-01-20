package org.scalair.monad

trait ChoiceBuilderTrait {
  type M[+Z] <: Choice[Z]
  def _nothing[Z]: M[Z]
  trait Choice[+A] { self: M[A] =>
    def or[B >: A](mb: => M[B]): M[B]
  }
}