package org.scalair.ir.transfer

import org.scalair.hoopl._
import org.scalair.hoopl.TypeDefines._
import org.scalair.ir.imir._
import collection.immutable.HashMap

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


trait ImIrInstTransferBwd[F<:Ordered[F]] extends ImIrInstTransfer[F] with BwdTransfer[ET[AbsNode],F] {
  import ET._
  def clAbsNode(fact:F)(x: ET[AbsNode]) = asT(x) match {
    case FirstNode(l, e) => fact
    case MiddleNode(n) => clInstruction(fact)(asA(x,n))
    case _ => throw new Exception()
  }

  def clLastNode(fact:FactBase[F])(x:ET[LastNode]):F = {
    throw new Exception ()
    /*
    val fact1 = clControlInst(fact)(asA(x, asT(x).inst))
    var mp = new HashMap[BlockId, F]()
    for (l <- asT(x).targets) mp = mp + (l -> fact1)
    mp
    */
  }

  def first:Block[ET[AbsNode],C,O]=>F=>F = { b => fact =>
    b match {
      case BFirst(n) => clAbsNode(fact)(n)
      case _ => throw new Exception()
    }
  }

  def middle:Block[ET[AbsNode],O,O]=>F=>F = { b => fact =>
    b match {
      case BMiddle(n) => clAbsNode(fact)(n)
      case _ => throw new Exception()
    }
  }

  def last:Block[ET[AbsNode],O,C]=>FactBase[F]=>F = { b => fact =>
    b match {
      case BLast(n) => clLastNode(fact)(n.asInstanceOf[ET[LastNode]])
      case _ => throw new Exception ()
    }
  }
}