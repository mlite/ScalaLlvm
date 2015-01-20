package org.scalair.ir.llvm

import org.scalair.ir.util.Emitter
import org.scalair.ir.common._

/**
 * User: wangn
 * Date: 4/16/11
 */

// Type
sealed abstract class Type(val name:String) extends Emitter  {
  def composeString(id:String) = id
  override def toString = name
}
case class I(val i:Int) extends Type("i" + i)
case class F(val i:Int) extends Type("f" + i)
case class V64() extends Type("v64")
case class V128() extends Type("v128")
case class VoidType() extends Type("void")
case class FloatType() extends Type("float")
case class DoubleType() extends Type("double")
case class FP128Type() extends Type("fp128")
case class X86FP80Type() extends Type("x86_fp80")
case class PPCFP128Type() extends Type("ppc_fp128")
case class X86MMXType() extends Type("x86mmx")
case class LabelType() extends Type("label")
case class MetadataType() extends Type("metadata")
case class OpaqueType() extends Type("opaque")
case class TypeName(x:String) extends Type(x)
case class TypeNo(val no:Int) extends Type("%"+no)
case class TypeUpRef(val no:Int) extends Type("\\"+ no)

case class ArrayType(val num_elements:Int, val baseType:Type) extends Type("array") {
  override def toString = "[ " + num_elements + " x " + baseType + " ]"
}
case class VectorType(val num_elements:Int, val baseType:Type) extends Type("vector") {
  override def toString = "< " + num_elements + " x " + baseType + " >"
}
case class StructType(val elementTypes:List[Type], val isPacked:Boolean) extends Type("struct") {
  override def toString = (if (isPacked) "<{ " else "{ ") + (elementTypes mkString ", ") +
    (if (isPacked) " }>" else " }")
}
case class PointerType(val baseType:Type, val addrspace:Option[AddrSpace]=None) extends Type("pointer") {
  override def toString = baseType + optToString (" ", addrspace) + "*"
}
case class FunType(val retType:Type, val formalParams:FormalParamList, val funAttrs:List[Attr])
  extends Type("function") {
  override def composeString(id:String) = retType + " " + id + formalParams + listToString(" ", funAttrs, " ", "")
  override def toString = composeString("")
}

case class FormalParam(val ty:Type, val attr1:List[Attr], val align:Option[Align], val id:Option[LocalVarOrID],
               val attr2:List[Attr]) extends Emitter {
  override def toString = ty + listToString(" ", attr1, " ", "") + optToString(" ", align) +
    optToString (" ", id) + listToString (" ", attr2, " ", "")
}

case class FormalParamList (val list:List[FormalParam], val hasDots:Boolean, val funAttrs:List[Attr]) extends Emitter {
  override def toString = "(" + listToString ("", list, ", ", "") +
    (if (hasDots) ((if (!list.isEmpty) "," else "") + "...") else "") + ")" +
    listToString (" ", funAttrs, " ", "")
}


trait Value extends Use with Emitter

class TV(val ty:Type, val v:Value) extends Use with Emitter {
  def uses:List[Use] = v.uses
  override def toString = ty + " " + v
}
object TV {
  def unapply(x:TV):Option[(Type, Value)] = Some((x.ty, x.v))
}
class TC(x1:Type, val x2:Const) extends TV(x1, x2)
object TC {
  def unapply(x:TC):Option[(Type,Const)] = Some((x.ty, x.x2))
}

// Variable Or ID
trait VarOrID extends Value { // extends VarOrIDOrConst {
  def uses:List[Use] = List(this)
}

sealed abstract class GlobalVarOrID() extends VarOrID {
}
case class GlobalID(val n:String) extends GlobalVarOrID {
  override def toString = "@" + n
}
case class GlobalVar(val n:String) extends GlobalVarOrID {
  override def toString = "@" + n
}

sealed abstract class LocalVarOrID() extends VarOrID {
}
case class LocalID(val n:String) extends LocalVarOrID {
  override def toString = "%" + n
}
case class LocalVar(val n:String) extends LocalVarOrID {
  override def toString = "%" + n
}
case class LocalStr(val n:QuoteStr) extends LocalVarOrID {
  override def toString = n.toString
}

// Constant Value
sealed abstract class Const() extends Value { //VarOrIDOrConst {
  def uses = List()
}
sealed abstract class AtomicConst (val name:String) extends Const { override def toString = name }
case class IntConst(str:String) extends AtomicConst(str)
case class FloatConst(str:String) extends AtomicConst(str)
case class NullValue() extends AtomicConst("null")
case class Undef() extends AtomicConst("undef")
case class BTrue() extends AtomicConst("true")
case class BFalse() extends AtomicConst("false")
case class Zero() extends AtomicConst("zeroinitializer")
case class GlobalAddr(val g:GlobalVarOrID) extends AtomicConst(g.toString) { override def toString = g.toString }
case class StructConst(val fields:List[TC], val isPacked:Boolean) extends Const  {
  override def toString = (if (isPacked) "<{" else "{") + (fields mkString ", ") + (if (isPacked) "}>" else "}")
}
case class VectorConst(val fields:List[TC]) extends Const { override def toString = "<" + (fields mkString ", ") + ">" }
case class ArrayConst(val elems:List[TC]) extends Const { override def toString = "[" + (elems mkString ", ") + "]" }
case class StrConst(val str:String) extends Const { override def toString = "c\"" + str + "\"" }
case class LabelQuoteStr(val str:QuoteStr) extends Const { override def toString = str.toString }
case class BlockAddress(val a1:GlobalVar, val a2:LocalVar) extends Const {
  override def toString = "blockaddress(" + a1 + ", " + a2 + ")"
}
case class ConstArithmaticExpr(val binOp:ArithmaticOp, val carry:List[Carry], val tv:TC, val op1:TC) extends Const {
  override def toString = binOp + listToString(" ", carry, " ", "") + "(" + tv + ", " + op1 + ")"
}
case class ConstBitwiseExpr(val op:BitwiseOp, val v1:TC, val v2:TC) extends Const {
  override def toString = op + "(" + v1 + ", " + v2 + ")"
}
case class ConstCast(val op:ConvOp, val v:TC, val t:Type) extends Const {
  override def toString = op + "(" + v + " to " + t + ")"
}
case class ConstGetElemPtr(val isInbounds:Boolean, val base:TC, val indices:List[TC]) extends Const {
  override def toString = "getelementptr " + (if (isInbounds) "inbounds " else "") +
    "(" + base + "," + listToString ("", indices, ", ", "") + ")"
}
case class ConstSelect(val cond:TC, val v1:TC, val v2:TC) extends Const {
  override def toString = "select (" + cond + ", " + v1 + ", " + v2 + ")"
}
case class ConstICmpExpr(val cmp:ICmpOp, val v1:TC, val v2:TC) extends Const {
  override def toString = "icmp " + cmp + "(" + v1 + ", " + v2 + ")"
}
case class ConstFCmpExpr(val cmp:FCmpOp, val v1:TC, val v2:TC) extends Const {
  override def toString = "fcmp " + cmp + "(" + v1 + ", " + v2 + ")"
}
case class ConstExtract (val tv:TC, val idx:Int) extends Const {
  override def toString = "extract (" + tv + ", " + idx + ")"
}
case class ConstInsertElement(val vect:TC, val tv:TC, val index:TC) extends Const {
  override def toString = "insertelement (" + tv + ", " + tv + ", " + index + ")"
}
case class ConstShuffleVector(val vect1:TC, val vect2:TC, val mask:TC) extends Const {
  override def toString = "shufflevector (" + vect1 + ", " + vect2 + ", " + mask + ")"
}
case class ConstExtractValue(val tv:TC, val indices:List[String]) extends Const {
  override def toString = "extractvalue (" + tv + listToString (", ", indices, ", ", "") + ")"
}
case class ConstInsertValue(val vect:TC, val tv:TC, val indices:List[String]) extends Const {
  override def toString = "insertvalue (" + vect + ", " + tv + listToString(", ", indices, ", ", "") + ")"
}
case class ConstExtractElement(val tv:TC, val index:TC) extends Const {
  override def toString = "extractelement (" + tv + ", " + index + ")"
}


case class NullTV() extends TC(PointerType(I(8)), NullValue()) {
  override def toString = "null"
}

//trait MetaValue extends Value
sealed abstract class MetaConst extends Const //with MetaValue
case class MDConst(c:Const) extends MetaConst { override def toString = "!" + c.toString }
case class MDString(n:QuoteStr) extends MetaConst { override def toString = "!" + n.toString }
case class MDNode(n:String) extends MetaConst { override def toString = "!" + n }
case class MDVar(n:String) extends MetaConst { override def toString = "!" + n }
case class MDRef(n:LocalVarOrID) extends MetaConst { override def toString = n.toString }



sealed case class Label(val s:String) extends Emitter { override def toString = s + ":" }

// RHS
abstract class RHS() extends Value

// RHS- Arithmaitc Expression etc
sealed abstract class Expr() extends RHS
case class ArithmaticExpr(val binOp:ArithmaticOp, val carry:List[Carry], val t:Type, val v1:Value, val v2:Value)
  extends Expr {
  override def toString = binOp + listToString(" ", carry, " ", "") + " " + t + " " + v1 + ", " + v2
  def uses = v1.uses:::v2.uses
}
case class ICmpExpr(val cmp:ICmpOp, val t:Type, val v1:Value, val v2:Value) extends Expr {
  override def toString = "icmp " + cmp + " " + t + " " + v1 + ", " + v2
  def uses = v1.uses:::v2.uses
}
case class FCmpExpr(val cmp:FCmpOp, val t:Type, val v1:Value, val v2:Value) extends Expr {
  override def toString = "fcmp " + cmp + " " + t + " " + v1 + ", " + v2
  def uses = v1.uses:::v2.uses
}
case class BitwiseExpr(val binOp:BitwiseOp, val t:Type, val v1:Value, val v2:Value) extends Expr {
  override def toString = binOp + " " + t + " " + v1 + ", " + v2
  def uses = v1.uses:::v2.uses
}
case class GetElemPtr(val tv:TV, val indices:List[TC]) extends Expr {
  override def toString = "getelementptr " + tv + listToString (", ", indices, ", ", "");
  def uses = tv.uses:::(indices.flatMap(_.uses))
}
case class GetResult(val tv:TV, val index:String) extends Expr { // remove in 3.0
  override def toString = "getresult " + tv + ", " + index
  def uses = tv.uses
}
case class CastExpr(val op:ConvOp, val tv:TV, val t:Type) extends Expr {
  override def toString = op + " " + tv + " to " + t
  def uses = tv.uses
}
case class Select (val c:TV, val t:TV, val v:TV) extends Expr {
  override def toString = "select " + c + ", " + t + ", " + v
  def uses = c.uses:::t.uses:::v.uses
}
case class Extract (val tv:TV, val idx:Int) extends Expr {
  override def toString = "extract " + tv + ", " + idx
  def uses = tv.uses
}

// RHS - Memory operations
sealed abstract class MemOp() extends RHS
case class Alloca(val ty:Type, val size:Option[TV], val align:Option[Align]) extends MemOp {
  override def toString = "alloca " + ty + optToString(", ", size) + optToString (", ", align)
  def uses = size.foldLeft(List[Use]())((p,x) => p:::x.uses)
}
case class Malloc(val ty:Type, val size:Option[TV], val align:Option[Align]) extends MemOp {
  override def toString = "malloc " + ty + optToString (", ", size) + optToString (", ", align)
  def uses = size.foldLeft(List[Use]())((p,x) => p:::x.uses)
}
case class Free(val tv:TV) extends MemOp {
  override def toString = "free " + tv
  def uses = tv.uses
}
case class Load(val volatile:Boolean, val ptr:TV, val align:Option[Align]) extends MemOp {
  override def toString = (if (volatile) "volatile " else "") + "load " + ptr + optToString (", ", align)
  def uses = ptr.uses
}
case class Store(val volatile:Boolean, val v:TV, val ptr:TV, val align:Option[Align]) extends MemOp {
  override def toString = (if (volatile) "volatile " else "") + "store " + v + ", " + ptr + optToString (", ", align)
  def uses = v.uses:::ptr.uses
}

// Inline Assembly
case class InlineAsm(val hasSideEffect:Boolean, val alignStack:Boolean, val s1:QuoteStr,
                     val s2:QuoteStr) extends Value {
  def uses = List()
  override def toString = "asm " + (if (hasSideEffect) "sideeffect " else "") +
    (if (alignStack) "alignstack " else "") + s1 + ", "  + s2
}


// RHS other expression
case class Phi(val ty:Type, val ins:List[(Value, Value)]) extends RHS {
  override def toString = "phi " + ty + " " +
    ins.tail.foldLeft("[" + ins.head._1 + "," + ins.head._2 + "]") ((p, e) => p + ", " + "[" + e._1 + ", " + e._2 + "]")

  def uses = ins.flatMap(x=> x._1.uses:::x._2.uses)
}

case class Call(val cconv:Option[CConv], val retAttrs:List[Attr], val retType:Type, val fn:VarOrID, val actualParams:ActualParamList,
                val funAttrs:List[Attr]) extends RHS {
  override def toString = "call" + optToString (" ", cconv) + listToString (" ", retAttrs, " ", "") + " " +
    retType + "  " + fn + actualParams + listToString(" ", funAttrs, " ", "")

  def uses = fn.uses:::actualParams.uses
}

case class ExtractElement(val tv:TV, val index:TV) extends RHS {
  override def toString = "extractelement " + tv + ", " + index
  def uses = tv.uses:::index.uses
}
case class InsertElement(val vect:TV, val tv:TV, val index:TV) extends RHS {
  override def toString = "insertelement " + vect + ", " + tv + ", " + index
  def uses = vect.uses:::tv.uses:::index.uses
}
case class ShuffleVector(val vect1:TV, val vect2:TV, val mask:TV) extends RHS {
  override def toString = "shufflevector " + vect1 + ", " + vect2 + ", " + mask
  def uses = vect1.uses:::vect2.uses:::mask.uses
}
case class ExtractValue(val tv:TV, val indices:List[String]) extends RHS {
  override def toString = "extractvalue " + tv + listToString (", ", indices, ", ", "")
  def uses = tv.uses
}
case class InsertValue(val vect:TV, val tv:TV, val indices:List[String]) extends RHS {
  override def toString = "insertvalue " + vect + ", " + tv + listToString (", ", indices, ", ", "")
  def uses = vect.uses:::tv.uses
}

case class VaArg(val tv:TV, val v:Type) extends RHS {
  override def toString = "va_arg " + tv + ", " + v
  def uses = tv.uses
}


abstract class AbsInst extends Use with Def with Emitter

// Instruction
case class Instruction(val left:Option[VarOrID], val rhs:RHS, val dbg:List[Dbg]) extends AbsInst {
  override def toString = "  " + optToString(left, " = ") + rhs.toString +
    (if (dbg.isEmpty) "" else (dbg.mkString (", ", ",", "")))
  def uses = rhs.uses
  def defined = left
}
case class Comment(val s:Any) extends AbsInst {
  def uses = List()
  def defined = None
  override def toString = "; " + s
}

// Terminator Instruction
sealed abstract class ControlInst extends Use with Emitter
case class Unreachable() extends ControlInst {
  override def toString = "unreachable"
  def uses = List()
}

case class Ret (val v:List[TV]) extends ControlInst {
  override def toString =
    if (v.isEmpty) "ret void"
    else "ret" + listToString (" ", v, ", ", "")

  def uses = v.flatMap(_.uses)
}
case class Br(val l:TV) extends ControlInst {
  override def toString = "br " + l
  def uses = l.uses
}
case class CBr(val cond:TV, val tru:TV, val fal:TV) extends ControlInst {
  override def toString = "br " + cond + ", " + tru + "," + fal
  def uses = cond.uses:::tru.uses:::fal.uses
}
case class IndirectBr(val cond:TV, val branchs:List[TV]) extends ControlInst {
  override def toString = "indirectbr " + cond + ", " + "[" + listToString ("", branchs, ", ", "") + "]"
  def uses = cond.uses:::branchs.flatMap(_.uses)
}
case class Switch(val v:TV, val default:TV, val jumptable:List[(TV, TV)]) extends ControlInst {
  override def toString = "switch " + v + "," + default + "[" +
    jumptable.foldLeft("")((p, e) => p + "\n" + e._1 + "," + e._2) + "]"

  def uses = v.uses:::default.uses:::jumptable.flatMap(x=> x._1.uses:::x._2.uses)
}
case class Invoke(val lhs:Option[VarOrID], val cconv:Option[CConv], val retAttrs:List[Attr],
                  val fn:TV, val actualParams:ActualParamList, val funAttrs:List[Attr],
                  val label:TV, val exception:TV) extends ControlInst with Def {
  override def toString = optToString(lhs, " = " ) +  "invoke" + optToString (" ", cconv) +
    listToString (" ", retAttrs, " ", " ") + fn + actualParams + listToString (" ", funAttrs, " ", "") +
    " " + "to " + label + " unwind " + exception
  def uses = fn.uses:::actualParams.uses:::label.uses:::exception.uses
  def defined = lhs
}

case class Unwind() extends ControlInst {
  override def toString = "unwind"
  def uses = List()
}

case class ControlInstDbg(val inst:ControlInst, val dbg:List[Dbg]) extends Use {
  override def toString = "  " + inst + (if (dbg.isEmpty) "" else (dbg.mkString (", ", ",", "")))
  def uses = inst.uses
}

case class ActualParam(val ty:Type, val attrs1:List[Attr], val align:Option[Align], val v:Value,
                              val attrs2:List[Attr]) extends Use with Emitter {
  override def toString = ty + listToString (" ", attrs1, " ", "") + " " + optToString (align, " ") + v +
    listToString (" ", attrs2, " ", "")
  def uses = v.uses
}

case class ActualParamList(val list:List[ActualParam]) extends Use with Emitter {
  override def toString = "(" + listToString ("", list, ", ", "") + ")"
  def uses = list.flatMap(_.uses)
}

case class Dbg(val s:String, val meta:MetaConst) {
  override def toString = "!" + s + "  " + meta
}


// Top level element in a module
trait TopLevel extends Emitter

// Alias
case class Alias(val lhs:Option[GlobalVarOrID], val visibility:Option[Visibility], val linkage:Option[Linkage],
                 val aliasee:Aliasee) extends TopLevel {
  override def toString = optToString(lhs, " = ") + optToString (visibility, " ") +  "alias " +
    optToString (linkage, " ") + aliasee.toString()
}

sealed abstract class Aliasee
case class AliaseeTV (tv:TV) extends Aliasee  { override def toString () = tv.toString }
case class AliaseeBitCast (tv:TV, t:Type) extends Aliasee {
  override def toString = "bitcast(" + tv + " to " + t + ")"
}
case class AliaseeGetElemPtr(get:ConstGetElemPtr) extends Aliasee { override def toString = get.toString() }

// Target
case class Target(val k:String, val v:QuoteStr) extends TopLevel { override def toString = "target " + k + " = " + v }
case class Declare(val header:FunctionHeader) extends TopLevel { override def toString = "declare " + header }
case class TypeDef(val name:String, val ty:Type) extends TopLevel { override def toString = name + " = type " + ty }
case class UnamedType(val id:Int, val ty:Type) extends TopLevel { override def toString = "type " + ty + "; " + id }

case class DepLibs(val l:List[QuoteStr]) extends TopLevel {
  override def toString = "deplibs = " + "[" + listToString ("", l, ", ", "") + "]"
}

case class Global(val lhs:Option[GlobalVarOrID], val linkage:Option[Linkage], val visiblilty:Option[Visibility],
                  val threadLocation:Boolean, val addrSpace:Option[AddrSpace], val globalType:GlobalType, val ty:Type,
                  val const:Option[Const], val section:Option[Section], val align:Option[Align]) extends TopLevel {
  override def toString = optToString (lhs, " = ") + optToString (linkage, " ") + optToString (visiblilty, " ") +
    optToString (addrSpace, " ") + globalType + " " + ty + optToString (" ", const) +
    optToString(", ", section) + optToString (", ", align)
}

case class ModuleAsm(val str:QuoteStr) extends TopLevel { override def toString = "module asm " + str }
case class DbgInit(val lhs:String, val start:Int) extends TopLevel
case class StandardaloneMD(val lhs:String, val ty:TV) extends TopLevel { override def toString = "!" + lhs + " = " + ty}
case class NamedMD(val lhs:MDVar, val rhs:List[MDNode]) extends TopLevel {
  override def toString = lhs + " = " + "!{" + (rhs mkString ", ") + "}"
}

// Function definition
case class FunctionDef (val funProto:FunctionHeader, val blocks:List[Block]) extends TopLevel  {
  override def toString = "define " + funProto + " {\n" + (blocks mkString "\n") + "\n}"
}

case class FunctionHeader (val linkage:Option[Linkage], val visibility:Option[Visibility],
                           val cconv:Option[CConv], val retAttrs:List[Attr],
                           val name:GlobalVarOrID, val funType:FunType,
                           val section:Option[Section], val align:Option[Align],
                           val gc:Option[GC]) extends Emitter {
  override def toString =
    optToString(linkage, " ") + optToString(visibility, " ") + optToString(cconv, " ") +
      listToString (" ", retAttrs, " ", " ") + funType.composeString(name.toString) +
      optToString (" ", section) + optToString (" ", align) + optToString (" ", gc)
}

case class Block(val label:Option[Label], val index:Int, val middle:List[AbsInst],
                 val end:ControlInstDbg) extends Use with Emitter {
  override def toString() =  optToString(label, "\n") + (middle mkString "\n") + "\n" + end.toString
  def uses = middle.flatMap(_.uses):::end.uses
}

// Module
case class Module(val list:List[TopLevel]) { override def toString = (list mkString "\n") }