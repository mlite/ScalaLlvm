package org.scalair.hoopl

import monad.{SimpleUniqueMonadBuilder, SimpleFuelMonadBuilder}
import TypeDefines._
/**
 * User: wangn
 * Date: 8/6/11
 */

trait AnalyzerAndRewriterBuilder[N<:NonLocal] {
  val passBuilder:PassBuilder[N]
  type F = passBuilder.F
}

trait AnalyzerAndRewriterFwdBuilder[N<:NonLocal]  { self =>
  val passBuilder:PassFwdBuilder[N]
  type F = passBuilder.F

  final def run(graph:Graph[N,O,C]):(Graph[N,O,C], passBuilder.Image) = {
    val m = Graph.analyzeAndRewriteFwd[O,C,N,passBuilder.F](graph, passBuilder.pass,
      graph.entries, Left(passBuilder.entry))

    val ret = SimpleFuelMonadBuilder.runWithFuel[(Graph[N,O,C], FactBase[passBuilder.F], MaybeO[C,passBuilder.F])](Int.MaxValue)(m)
    val (g, fb, x):(Graph[N,O,C], FactBase[passBuilder.F], MaybeO[O,passBuilder.F]) =
      SimpleUniqueMonadBuilder.runSimpleUniqueMonad[(Graph[N,O,C], FactBase[passBuilder.F], MaybeO[C,passBuilder.F])](ret)
    (g, passBuilder.toImage(fb))
  }
}


trait AnalyzerAndRewriterBwdBuilder[N<:NonLocal]  { self =>
  val passBuilder:PassBwdBuilder[N]
  type F = passBuilder.F

  final def run(graph:Graph[N,O,C]):(Graph[N,O,C], passBuilder.Image) = {
    val m = Graph.analyzeAndRewriteBwd[O,C,N,passBuilder.F](graph, passBuilder.pass,
      graph.entries, Left(passBuilder.entry))

    val ret = SimpleFuelMonadBuilder.runWithFuel[(Graph[N,O,C], FactBase[passBuilder.F], MaybeO[O,passBuilder.F])](Int.MaxValue)(m)
    val (g, fb, x):(Graph[N,O,C], FactBase[passBuilder.F], MaybeO[O,passBuilder.F]) =
      SimpleUniqueMonadBuilder.runSimpleUniqueMonad[(Graph[N,O,C], FactBase[passBuilder.F], MaybeO[O,passBuilder.F])](ret)
    (g, passBuilder.toImage(fb))
  }
}