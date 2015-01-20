package org.scalair.ir.transfer

import org.scalair.ir.imir.{InlineAsm, Const, VarOrID, Value,ET}

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 10/26/11
 * Time: 9:53 PM
 * To change this template use File | Settings | File Templates.
 */

trait ImIrValueTransfer[F] extends ImIrTypeAndConstTransfer[F] {
  import ET._
  val vis: ValueVisitor[F]

  def clValue(env:F)(x:ET[Value]):F =  {
    asT(x) match {
      case v:VarOrID => clVarOrID(env)(asA(x,v))
      case v:Const => clConst(env)(asA(x,v))
      case v:InlineAsm => clInlineAsm(env)(asA(x,v))
    }
  }
  def clInlineAsm(env:F)(x:ET[InlineAsm]):F = if (vis.visitInlineAsm(env)(x)) {
    asT(x) match {
      case InlineAsm(h, a, s1, s2) => env
    }
  } else env

  trait ValueVisitor[F] extends TypeAndConstVisitor[F] {
    def visitInlineAsm(fact: F)(x: ET[InlineAsm]): Boolean = true
    def bcInlineAsm(fact: F)(x: ET[InlineAsm]): F = fact
    def acInlineAsm(fact: F)(x: ET[InlineAsm]): F = fact
  }
}