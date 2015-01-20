package org.scalair.hoopl.monad

import org.scalair.monad.MonadBuilderTrait

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 10/1/11
 * Time: 7:29 AM
 * To change this template use File | Settings | File Templates.
 */

trait FuelMonadBuilderTrait extends MonadBuilderTrait {
  type Fuel = Int
  type FuelMonad[+Z] = M[Z]
  def getFuel:FuelMonad[Fuel]
  def setFuel:Fuel => FuelMonad[Unit]
  final def withFuel[A]:Option[A] => FuelMonad[Option[A]] = { x:Option[A] =>
    x match {
      case None => _return(None)
      case Some(a) => {
        getFuel flatMap { f  =>
          println("withFuel " + f)
          if (f == 0) _return(None)
          else setFuel(f-1) >> _return(Some(a))
        }
      }
    }
  }
}