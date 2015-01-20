package org.scalair.monad

/**
 * User: wangn
 * Date: 9/26/11
 */

trait StateTidentityMonadBuilderTrait extends StateTmonadBuilderTrait {
  override val monadBuilder = IdentityMonadBuilder
  type MonadBuilderType = IdentityMonadBuilder.type
}