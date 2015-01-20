package org.scalair.hoopl.monad

import org.scalair.monad.MonadBuilderTrait

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 10/1/11
 * Time: 5:09 AM
 * To change this template use File | Settings | File Templates.
 */

trait CheckpointMonadBuilderTrait extends MonadBuilderTrait {
  type CheckpointMonad[+Z] = M[Z]
  type Checkpoint
  def checkpoint:CheckpointMonad[Checkpoint]
  def restart:Checkpoint => CheckpointMonad[Unit]
}