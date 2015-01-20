package org.scalair.ir.transfer

import org.scalair.ir.imir._

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


trait ImIrVarOrIdTransfer[F]  { self =>
  import ET._
  /*
  type A[T] = (ImEnv, T)

  def asT[T](a:A[T]) = a._2
  def asA[T](x:A[_],t:T)= (x._1, t)
  */

  val vis:VarOrIdVisitor[F]

  def error = throw new Exception ()

  def clVarOrID(env:F)(x:ET[VarOrID]):F = asT(x) match {
    case v:GlobalVarOrID => clGlobalVarOrID(env)(asA(x,v))
    case v:LocalVarOrID => clLocalVarOrID(env)(asA(x,v))
  }

  def clGlobalVarOrID(env:F)(x:ET[GlobalVarOrID]):F = asT(x) match {
    case v:GlobalID  => vis.bcGlobalID(env)(asA(x,v)); vis.acGlobalID(env)(asA(x,v))
    case v:GlobalVar => vis.bcGlobalVar(env)(asA(x,v)); vis.acGlobalVar(env)(asA(x,v))
  }

  def clLocalVarOrID(env:F)(x:ET[LocalVarOrID]):F = asT(x) match {
    case v:LocalID => vis.bcLocalID(env)(asA(x,v)); vis.acLocalID(env)(asA(x,v))
    case v:LocalVar => vis.bcLocalVar(env)(asA(x,v)); vis.acLocalVar(env)(asA(x,v))
    case v:LocalStr => vis.bcLocalStr(env)(asA(x,v)); vis.acLocalStr(env)(asA(x,v))
  }

  def clLocalVar(env:F)(x:ET[LocalVar]):F = {
    vis.bcLocalVar(env)(x)
    vis.acLocalVar(env)(x)
  }

  def clGlobalVar(env:F)(x:ET[GlobalVar]):F = {
    vis.bcGlobalVar(env)(x)
    vis.acGlobalVar(env)(x)
  }

  trait VarOrIdVisitor[F] {
    def visitGlobalVar(fact:F)(x:ET[GlobalVar]):Boolean = true
    def bcGlobalVar(fact:F)(x:ET[GlobalVar]):F = fact
    def acGlobalVar(fact:F)(x:ET[GlobalVar]):F = fact

    def visitGlobalID(fact:F)(x:ET[GlobalID]):Boolean = true
    def bcGlobalID(fact:F)(x:ET[GlobalID]):F = fact
    def acGlobalID(fact:F)(x:ET[GlobalID]):F = fact

    def visitLocalVar(fact:F)(x:ET[LocalVar]):Boolean = true
    def bcLocalVar(fact:F)(x:ET[LocalVar]):F = fact
    def acLocalVar(fact:F)(x:ET[LocalVar]):F = fact

    def visitLocalStr(fact:F)(x:ET[LocalStr]):Boolean = true
    def bcLocalStr(fact:F)(x:ET[LocalStr]):F = fact
    def acLocalStr(fact:F)(x:ET[LocalStr]):F = fact

    def visitLocalID(fact:F)(x:ET[LocalID]):Boolean = true
    def bcLocalID(fact:F)(x:ET[LocalID]):F = fact
    def acLocalID(fact:F)(x:ET[LocalID]):F = fact
  }
}