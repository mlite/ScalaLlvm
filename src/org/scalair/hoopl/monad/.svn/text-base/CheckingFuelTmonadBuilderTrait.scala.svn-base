package org.scalair.hoopl.monad

import org.scalair.monad.StateTmonadBuilderTrait

/**
 * User: wangn
 * Date: 10/1/11
 */

trait CheckingFuelTmonadBuilderTrait extends StateTmonadBuilderTrait
with CheckpointMonadBuilderTrait
with UniqueMonadBuilderTrait
with FuelMonadBuilderTrait {
  type MonadBuilderType <: CheckpointMonadBuilderTrait with UniqueMonadBuilderTrait

  type S = Fuel
  override type CheckpointMonad[+Z] = M[Z]
  override type UniqueMonad[+Z] = M[Z]
  override type FuelMonad[+Z] = M[Z]

  type Checkpoint = (Fuel, monadBuilder.Checkpoint)

  def checkpoint =
    _closeT[(Fuel, monadBuilder.Checkpoint)] {
      fuel => monadBuilder.checkpoint flatMap {s =>
        monadBuilder._return(((fuel, s), fuel))
      }
    }

  def restart:Checkpoint => CheckpointMonad[Unit] = {
    x:(Fuel, monadBuilder.Checkpoint) =>
      val (fuel, s) = x
      _closeT[Unit] {_ =>
        monadBuilder.restart(s) flatMap {_ => monadBuilder._return(((),fuel))}
      }
  }

  def runWithFuel[A]:Fuel => M[A] => MonadBuilderType#M[A] = {
    fuel:Fuel => m:M[A] =>
      m.openT(fuel) flatMap { x => monadBuilder._return(x._1) }
  }

  def getFuel = _get(())
  def setFuel = _set

  def freshUnique =
    _closeT[Unique] {
      f =>  monadBuilder.freshUnique flatMap {l => monadBuilder._return((l, f))}
    }
}