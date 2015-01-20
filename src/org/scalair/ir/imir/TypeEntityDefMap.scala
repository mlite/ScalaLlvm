package org.scalair.ir.imir

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 5/7/11
 * Time: 11:29 PM
 * To change this template use File | Settings | File Templates.
 */

class TypeEntityDefMap(val dataLayout:DataLayout) extends EntityDefMap[Type](Map(), Map()) {
  def sizeOfInBit(ty:Type):Int = ty match {
    case x:PrimitiveType => x.bits
    case x:PointerType => dataLayout.ptrSize
    case _ => throw new Exception("getTypeSizeInBit " + ty)
  }

  def sizeOfInByte(ty:Type):Int = 4

  def canonicalize(ty:Type):Type = {
    def f(fp:FormalParam) = FormalParam(canonicalize(fp.ty), fp.attr1, fp.align, None, fp.attr2)

    ty match {
      case TypeName(s) => getDef(s) match {
        case Some(rhs) => rhs
        case None => throw new Exception("bad")
      }
      case TypeNo(n) => getDef(n) match {
        case Some(rhs) => rhs
        case None => throw new Exception("bad")
      }
      case x:PrimitiveType => x
      case ArrayType(n, ety) => ArrayType(n, canonicalize(ety))
      case StructType(l, isPacked) => StructType(l.map(canonicalize(_)), isPacked)
      case VectorType(n, ety) => VectorType(n, canonicalize(ety))
      case PointerType(ety, addrSpace) => PointerType(canonicalize(ety), addrSpace)
      case FunType(retTy, fps, funAtts) =>
        FunType(canonicalize(retTy), FormalParamList(fps.list.map(f), fps.hasDots, fps.funAttrs), funAtts)
      case _ => throw new Exception()
    }
  }
}