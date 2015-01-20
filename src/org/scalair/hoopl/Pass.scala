package org.scalair.hoopl

import TypeDefines._

/**
 * User: wangn
 * Date: 7/17/11
 */

trait Pass[F<:Ordered[F]] extends HoopMonad { self =>
  val lattice:DataflowLattice[F]
  def isFwd:Boolean
}


