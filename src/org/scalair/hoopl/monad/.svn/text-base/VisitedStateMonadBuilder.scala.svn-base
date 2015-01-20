package org.scalair.hoopl.monad

import org.scalair.monad.StateTidentityMonadBuilderTrait
import org.scalair.hoopl.TypeDefines._

/**
 * User: wangn
 * Date: 9/28/11
 */

object VisitedStateMonadBuilder extends StateTidentityMonadBuilderTrait {
  type S = BlockIdSet
  def marked(bid:BlockId) = {
    _closeT { visited =>
      monadBuilder._return((visited.contains(bid), visited))
    }
  }
  def mark(bid:BlockId) = {
    _closeT { visited =>
      monadBuilder._return(((), visited + bid))
    }
  }
}