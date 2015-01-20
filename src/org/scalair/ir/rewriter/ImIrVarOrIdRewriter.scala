package org.scalair.ir.rewriter

import org.scalair.ir.imir._
import org.scalair.ir.ImEnvState

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


trait ImIrVarOrIdRewriter[S,F] extends ImEnvState[S,F] {
  def rwVarOrID(env:F)(x:A[VarOrID]):A[VarOrID] = x flatMap {
    _ match {
      case Left(v:GlobalVarOrID) => rwGlobalVarOrID(env)(left(v))
      case Left(v:LocalVarOrID) => rwLocalVarOrID(env)(left(v))
    }
  }

  def rwGlobalVarOrID(env:F)(x:A[GlobalVarOrID]):A[GlobalVarOrID] = x flatMap {
    _ match {
      case Left(v:GlobalID)  => rwGlobalID(env)(left(v))
      case Left(v:GlobalVar) => rwGlobalVar(env)(left(v))
    }
  }

  def rwLocalVarOrID(env:F)(x:A[LocalVarOrID]):A[LocalVarOrID] = x flatMap {
    _ match {
      case Left(v:LocalID) => rwLocalID(env)(left(v))
      case Left(v:LocalVar) => rwLocalVar(env)(left(v))
      case Left(v:LocalStr) => rwLocalStr(env)(left(v))
    }
  }

  def rwLocalID(env:F)(x:A[LocalID]):A[LocalID] = x
  def rwLocalVar(env:F)(x:A[LocalVar]):A[LocalVar] = x
  def rwLocalStr(env:F)(x:A[LocalStr]):A[LocalStr] = x

  def rwGlobalID(env:F)(x:A[GlobalID]):A[GlobalID] = x
  def rwGlobalVar(env:F)(x:A[GlobalVar]):A[GlobalVar] = x
}