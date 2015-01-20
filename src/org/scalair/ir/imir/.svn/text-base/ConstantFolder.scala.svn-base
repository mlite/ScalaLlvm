package org.scalair.ir.imir

import org.scalair.ir.common._
import org.scalair.hoopl.TypeDefines.OrderedMap
import org.scalair.hoopl.Pointed

/**
 * User: wangn
 * Date: 10/27/11
 */

object ConstantFolder {
  import ET._

  def intArithmatic(binOp:ArithmaticOp, carry:List[Carry], op1:String, op2:String):String = {
    val i1 = op1.toInt
    val i2 = op2.toInt
    val result = binOp match {
      case Add => i1 + i2
      case Sub => i1 - i2
      case Mul => i1 * i2
      case UDiv => i1 / i2
      case SDiv => i1 / i2
      case URem => i1 % i2
      case SRem => i1 % i2
      case _ => throw new Exception()
    }
    result.toString
  }

  def floatArithmatic(binOp:ArithmaticOp, carry:List[Carry], op1:String, op2:String):String = {
    val i1 = op1.toFloat
    val i2 = op2.toFloat
    val result = binOp match {
      case FAdd => i1 + i2
      case FSub => i1 - i2
      case FMul => i1 * i2
      case FDiv => i1 / i2
      case FRem => i1 % i2
      case _ => throw new Exception()
    }
    result.toString
  }

  def foldValue(fact:OrderedMap[VarOrID, Pointed[Const]])(x:ET[Value]):Option[Const] = {
    asT(x) match {
      case ArithmaticExpr (bop,c,t,op1,op2) => {
        (op1, op2) match {
          case (c1:Const, c2:Const) => Some(foldConst(asA(x, ConstArithmaticExpr(bop,c, c1, c2))))
          case (_, _) => {
            val c1 = foldValue(fact)(asA(x,op1))
            val c2 = foldValue(fact)(asA(x,op2))
            (c1,c2) match {
              case (Some(c10), Some(c20)) => Some(foldConst(asA(x, ConstArithmaticExpr(bop,c,c10, c20))))
              case (None,None) => None
            }
          }
        }
      }
      case c:Const => Some(c)
      case x:VarOrID => fact.get(x) match {
        case Some(v) => v.getOrNone
        case None => None
      }
      case _ => throw new Exception()
    }
  }

  def foldConst(x:ET[Const]):Const = {
    val x1 = asT(x)
    x1 match {
      case GlobalAddr(g) => x1
      case PrimitiveConst(_, v) => x1
      case v: MetaConst => v
      case StructConst(f, isPacked) => x1
      case VectorConst(f) => x1
      case ArrayConst(e) => x1
      case StrConst(str) => x1
      case LabelQuoteStr(str) => x1
      case BlockAddress(a1, a2) => x1
      case ConstArithmaticExpr(bop, c, tv, op1) => {
        val e1 = foldConst(asA(x,tv));
        val e2 = foldConst(asA(x,op1));
        (e1, e2) match {
          case (PrimitiveConst(t1,v1), PrimitiveConst(t2,v2)) if (t1 == t2) => {
            val result = (v1, v2) match {
              case (IntConst(i1), IntConst(i2)) => IntConst(intArithmatic(bop,c,i1,i2))
              case (FloatConst(f1), FloatConst(f2)) => FloatConst(floatArithmatic(bop,c,f1,f2))
              case (_,_) => throw new Exception()
            }
            PrimitiveConst(t1,result)
          }
          case _ => x1
        }
      }
      case ConstBitwiseExpr(bop, op1, op2) => {
        val e1 = foldConst(asA(x,op1))
        val e2 = foldConst(asA(x,op2))
        x1
      }
      case ConstCast(conv, tv, t) => {
        x1
      }
      case ConstGetElemPtr(isInbounds, base, l) => {
        x1
      }
      case ConstSelect(cond, v1, v2) => {
        x1
      }
      case ConstICmpExpr(cmp, tv, v) => {
        x1
      }
      case ConstFCmpExpr(cmp, tv, v) => {
        x1
      }
      case ConstExtract(tv, idx) => x1
      case ConstInsertElement(vect, tv, index) => {
        x1
      }
      case ConstShuffleVector(vect1, vect2, mask) => {
        x1
      }
      case ConstExtractValue(tv, indices) => x1
      case ConstInsertValue(vect, tv, indices) => {
        x1
      }
      case ConstExtractElement(tv, index) => {
        x1
      }
    }
  }
}