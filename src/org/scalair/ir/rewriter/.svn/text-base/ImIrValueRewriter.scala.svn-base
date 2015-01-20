package org.scalair.ir.rewriter

import org.scalair.ir.imir._


/**
 * User: wangn
 * Date: 10/25/11
 */

trait ImIrValueRewriter[S,F] extends ImIrTypeAndConstRewriter[S,F] {
  def rwValue(env:F)(x:A[Value]):A[Value] = x flatMap {
    _ match {
      case Left(v:VarOrID) => rwVarOrID(env)(left(v))
      case Left(v:Const) => rwConst(env)(left(v))
      case Left(v:InlineAsm) => rwInlineAsm(env)(left(v))
    }
  }
  def rwInlineAsm(env:F)(x:A[InlineAsm]):A[InlineAsm] = x
}