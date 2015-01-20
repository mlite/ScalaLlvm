package org.scalair.monad

trait ComputationBuilderTrait {
  type M[+Z] <: Computation[Z]
  type In[-Z]
  type Out[+Z]
  trait Computation[+A] { self: M[A] =>
    def run[B >: A](i: In[B]): Out[B]
  }
}