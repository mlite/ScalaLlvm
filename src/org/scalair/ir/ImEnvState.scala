package org.scalair.ir

import org.scalair.ir.imir.ImEnv
import org.scalair.monad.{StateTidentityMonadBuilderTrait, OptionMonadBuilder, StateTmonadBuilderTrait, StateMonadBuilderTrait}

/**
 * User: wangn
 * Date: 10/16/11
 */

trait ImEnvState[S1, F] {
  val builder = new StateTidentityMonadBuilderTrait {
    type S = S1
    //override val monadBuilder = OptionMonadBuilder
    //type MonadBuilderType = OptionMonadBuilder.type
    def copy[A](a:A):M[A] = _closeT[A] { s:S => _fromReturn((a, s)) }
  }
  type A[T] = builder.M[Either[T,T]]

  def unit[T](x:T):A[T] = left(x)
  def left[T](x:T):A[T] = builder._return(Left(x))
  def right[T](x:T):A[T] = builder._return(Right(x))
  def ident[T](x:T):A[T] = builder.copy(Left(x))
}