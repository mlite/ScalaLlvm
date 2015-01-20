package org.scalair.hoopl

import TypeDefines._

/**
 * User: wangn
 * Date: 8/6/11
 */


trait PassBuilder[N<:NonLocal] extends HoopMonad { self =>
  // step 1. define the basic fact

  type F <: Ordered[F]

  // step 2. define the lattice
  val lattice:DataflowLattice[F]

  // step 3: define the entry value
  val entry:F

  // override only if you want to
  // convert fact base to your own
  // representation
  type Image
  def toImage(fb:FactBase[F]):Image
}

trait PassFwdBuilder[N<:NonLocal] extends PassBuilder[N] { self =>

  // step 4. define transfer function on lattice
  val transfer:FwdTransfer[N,self.F]

  // step 5. define the rewrite function based on lattice
  val rewrite:FwdRewrite[N,self.F]

  final lazy val pass:FwdPass[N,F] =
    new FwdPass[N,F] {
      val lattice = self.lattice
      val transfer = self.transfer
      val rewrite = HooplFwdRewrite.mkRewrite3[N,F](self.rewrite)
    }
}

trait PassBwdBuilder[N<:NonLocal] extends PassBuilder[N] { self =>

  // step 4. define transfer function on lattice
  val transfer:BwdTransfer[N,self.F]

  // step 5. define the rewrite function based on lattice
  val rewrite:BwdRewrite[N,self.F]

  final lazy val pass:BwdPass[N,F] =
    new BwdPass[N,F] {
      val lattice = self.lattice
      val transfer = self.transfer
      val rewrite = HooplBwdRewrite.mkRewrite3[N,F](self.rewrite)
    }
}