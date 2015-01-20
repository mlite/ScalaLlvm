package org.scalair.hoopl.monad

import org.scalair.monad.{StateTmonadBuilderTrait}

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 10/1/11
 * Time: 5:10 AM
 * To change this template use File | Settings | File Templates.
 */

trait UniqueTmonadBuilderTrait extends StateTmonadBuilderTrait with UniqueMonadBuilderTrait with CheckpointMonadBuilderTrait { self =>
  type S = LazyList
  override type UniqueMonad[+Z] = M[Z]
  override type CheckpointMonad[+Z] = M[Z]
  type Checkpoint = LazyList

  def freshUnique = _closeT { x:LazyList =>
    x match {
      case LazyList(hd,tail) => _fromReturn((hd, tail))
      case _ => throw new Exception("empty lazy list")
    }
  }

  def checkpoint = _get(())
  def restart = _set
}