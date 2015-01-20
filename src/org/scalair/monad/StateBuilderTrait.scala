package org.scalair.monad

trait StateBuilderTrait {
  type S
  type M[+Z] <: State[Z]
  def _get: Unit => M[S] // return the current state as a value
  def _set: S => M[Unit] // set S as the current state

  // update the current state with
  // the function 'f' and set value
  // as unit
  def _modify: (S => S) => M[Unit]

  trait State[+A] { self: M[A] =>
  }
}