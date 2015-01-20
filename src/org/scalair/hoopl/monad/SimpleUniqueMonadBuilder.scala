package org.scalair.hoopl.monad

import org.scalair.monad.IdentityMonadBuilder

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 10/1/11
 * Time: 7:06 AM
 * To change this template use File | Settings | File Templates.
 */

object SimpleUniqueMonadBuilder extends UniqueTmonadBuilderTrait {
  override val monadBuilder = IdentityMonadBuilder
  type MonadBuilderType = IdentityMonadBuilder.type

  def runSimpleUniqueMonad[A](s:M[A]):A = s.runState(LazyList(0))(())._1
}