package org.scalair.ir.imir

import org.scalair.ir.util.Emitter
import org.scalair.ir.common._
import org.scalair.hoopl.{O, C, Graph, NonLocal}

/**
 * User: wangn
 * Date: 4/16/11
 */

final case class TMatrics(val abiAlign:Int, val prefAlign:Int, val size:Option[Int]=None) {
}

final case class DataLayout() {
  val isBigEndian:Boolean = false
  val matrics:Map[String, TMatrics] = Map(
    "p" -> TMatrics(32, 32, Some(32)),
    "i1" -> TMatrics(8, 8),
    "i8" -> TMatrics(8, 8),
    "i16" -> TMatrics(16, 16),
    "i32" -> TMatrics(32, 32),
    "i64" -> TMatrics(64, 64),
    "f32" -> TMatrics(32, 32),
    "f64" -> TMatrics(32, 64),
    "v64" -> TMatrics(64, 64),
    "v128" -> TMatrics(128, 128),
    "a0" -> TMatrics(0, 64),
    "f80" -> TMatrics(32, 32))
  val intSize = List(8, 16, 32)

  def ptrSize = matrics.get("p") match {
    case Some(x) => x.size.getOrElse(32)
    case None => throw new Exception ("bad")
  }
}
// Type
sealed abstract class Type extends Ordered[Type] with Emitter {
  def composeString(id:String) = id
  def compare(that:Type):Int = this.toString.compare(that.toString)
}
sealed abstract class PrimitiveType(val bits:Int) extends Type {
}
case class I(n:Int) extends PrimitiveType(n) {
  override def toString = "i"+bits
}
sealed abstract class Vector(n:Int) extends PrimitiveType(n) {
  override def toString = "v"+bits
}
case class V64() extends Vector(64)
case class V128() extends Vector(128)
case class VoidType() extends Type {
  override def toString = ("void")
}

sealed abstract class FloatingType(n:Int) extends PrimitiveType(n)
case class F(n1:Int) extends FloatingType(n1) {
  override def toString = ("f"+bits)
}
case class FloatType() extends FloatingType(32) {
  override def toString = ("float")
}
case class DoubleType() extends FloatingType(64) {
  override def toString = ("double")
}
case class FP128Type() extends FloatingType(128) {
  override def toString = ("fp128")
}
case class X86FP80Type() extends FloatingType(80) {
  override def toString = ("x86_fp80")
}
case class PPCFP128Type() extends FloatingType(128) {
  override def toString = ("ppc_fp128")
}
case class X86MMXType() extends Type {
  override def toString = ("x86mmx")
}
case class LabelType() extends Type {
  override def toString = ("label")
}
case class MetadataType() extends Type {
  override def toString = ("metadata")
}
case class OpaqueType() extends Type {
  override def toString = ("opaque")
}
case class TypeName(x:String) extends Type {
  override def toString = (x)
}
case class TypeNo(val no:Int) extends Type {
  override def toString = ("%"+no)
}
case class TypeUpRef(val no:Int) extends Type {
  override def toString = ("\\"+no)
}
case class ArrayType(val num_elements:Int, val baseType:Type) extends Type {
  override def toString = ("[ " + num_elements + " x " + baseType + " ]")
}
case class VectorType(val num_elements:Int, val baseType:Type) extends Type {
  override def toString = ("< " + num_elements + " x " + baseType + " >")
}
case class StructType(val elementTypes:List[Type], val isPacked:Boolean = false) extends Type {
  override def toString =
    ((if (isPacked) "<{ " else "{ ") + (elementTypes mkString ", ") + (if (isPacked) " }>" else " }"))
}
case class PointerType(val baseType:Type, val addrspace:Option[AddrSpace] = None) extends Type {
  override def toString = (baseType + optToString (" ", addrspace) + "*")
}
case class FunType(val retType:Type, val formalParams:FormalParamList, val funAttrs:List[Attr])
  extends Type {
  override def composeString(id:String) = retType + " " + id + formalParams + listToString(" ", funAttrs, " ", "")
  override def toString = (composeString(""))
}
case class FormalParam(val ty:Type, val attr1:List[Attr], val align:Option[Align], val id:Option[LocalVarOrID],
                       val attr2:List[Attr]) extends Emitter {
  override def toString =
    (ty + listToString(" ", attr1, " ", "") + optToString(" ", align) +
      optToString (" ", id) + listToString (" ", attr2, " ", ""))
}
case class FormalParamList (val list:List[FormalParam], val hasDots:Boolean, val funAttrs:List[Attr]) extends Emitter {
  override def toString =
    ("(" + listToString ("", list, ", ", "") +
      (if (hasDots) ((if (!list.isEmpty) "," else "") + "...") else "") + ")" +
      listToString (" ", funAttrs, " ", ""))

  def types = list.map(_.ty)
}


trait Value extends Use with Emitter

// Variable Or ID
trait VarOrID extends Value with Ordered[VarOrID] {
  def uses:List[Use] = List(this)
  def compare(that:VarOrID):Int = this.toString.compare(that.toString)
}

trait GlobalVarOrID extends VarOrID {
  def pureImg:String
}
case class GlobalID(val n:Int) extends GlobalVarOrID {
  override def toString = "@" + n
  def pureImg = n.toString
}
case class GlobalVar(val n:String) extends GlobalVarOrID {
  override def toString = "@" + n
  def pureImg = n
}

trait LocalVarOrID extends VarOrID
case class LocalID(val n:Int) extends LocalVarOrID {
  override def toString = "%" + n
}
case class LocalVar(val n:String) extends LocalVarOrID {
  override def toString = "%" + n
}
case class LocalStr(val n:QuoteStr) extends LocalVarOrID {
  override def toString = (n.toString)
}

// Constant Value
sealed abstract class Const() extends Value with Ordered[Const] {
  def uses = List[Use]()
  def compare(that:Const):Int = this.toString.compare(that.toString)
}
sealed abstract class ConstLiteral (val name:String) {
  override def toString = name
}
case class MConst(t:Type) extends ConstLiteral("") {
  override def toString = "dsm1of(" + t + ")"
}
case class IntConst(str:String) extends ConstLiteral(str) {
}
case class FloatConst(str:String) extends ConstLiteral(str) {
}
case class NullValue() extends ConstLiteral("null") {
}
case class Undef() extends ConstLiteral("undef")
case class BTrue() extends ConstLiteral("true")
case class BFalse() extends ConstLiteral("false")
case class Zero() extends ConstLiteral("zeroinitializer")
case class SizeOf(val t:Type) extends ConstLiteral("sizeof(" + t + ")")

case class PrimitiveConst (typ:Type, v:ConstLiteral) extends Const {
  override def toString = typ.toString + " " + v.toString
}
case class GlobalAddr(val g:GlobalVarOrID) extends Const {
  override def toString = g.toString
}
sealed abstract class AggregateConst(val list:List[Const]) extends Const
case class StructConst(val fields:List[Const], isPacked:Boolean = false) extends AggregateConst(fields)  {
  override def toString =
    ((if (isPacked) "<{" else "{") + (fields mkString ", ") + (if (isPacked) "}>" else "}"))
}
case class VectorConst(val fields:List[Const]) extends AggregateConst(fields) {
  override def toString = "<" + (fields mkString ", ") + ">"
}
case class ArrayConst(val elems:List[Const]) extends AggregateConst(elems) {
  override def toString = "[" + (elems mkString ", ") + "]"
}
case class StrConst(val str:String) extends Const {
  override def toString = ("c\"" + str + "\"")
}
case class LabelQuoteStr(val str:QuoteStr) extends Const {
  override def toString = (str.toString)
}
case class BlockAddress(val a1:GlobalVar, val a2:LocalVar) extends Const {
  override def toString = ("blockaddress(" + a1 + ", " + a2 + ")")
}
case class ConstArithmaticExpr(val binOp:ArithmaticOp, val carry:List[Carry], val op1:Const, val op2:Const)
  extends Const {
  override def toString = (binOp + listToString(" ", carry, " ", "") + "(" + op1 + ", " + op2 + ")")
}
case class ConstBitwiseExpr(val op:BitwiseOp, val v1:Const, val v2:Const) extends Const {
  override def toString = (op + "(" + v1 + ", " + v2 + ")")
}
case class ConstCast(val op:ConvOp, val v:Const, val t:Type) extends Const {
  override def toString = (op + "(" + v + " to " + t + ")")
}
case class ConstGetElemPtr(val isInbounds:Boolean, val base:Const, val indices:List[Const]) extends Const {
  override def toString =
    ("getelementptr " + (if (isInbounds) "inbounds " else "") + "(" + base + ", " + listToString ("", indices, ", ", "") + ")")
}
case class ConstSelect(val cond:Const, val v1:Const, val v2:Const) extends Const {
  override def toString = ("select (" + cond + ", " + v1 + ", " + v2 + ")")
}
case class ConstICmpExpr(val cmp:ICmpOp, val v1:Const, val v2:Const) extends Const {
  override def toString = ("icmp " + cmp + "(" + v1 + ", " + v2 + ")")
}
case class ConstFCmpExpr(val cmp:FCmpOp, val v1:Const, val v2:Const) extends Const {
  override def toString = ("fcmp " + cmp + "(" + v1 + ", " + v2 + ")")
}
case class ConstExtract (val tv:Const, val idx:Int) extends Const {
  override def toString = ("extract (" + tv + ", " + idx + ")")
}
case class ConstInsertElement(val vect:Const, val tv:Const, val index:Const) extends Const {
  override def toString = ("insertelement (" + tv + ", " + tv + ", " + index + ")")
}
case class ConstShuffleVector(val vect1:Const, val vect2:Const, val mask:Const) extends Const {
  override def toString = ("shufflevector (" + vect1 + ", " + vect2 + ", " + mask + ")")
}
case class ConstExtractValue(val tv:Const, val indices:List[String]) extends Const {
  override def toString = ("extractvalue (" + tv + listToString (", ", indices, ", ", "") + ")")
}
case class ConstInsertValue(val vect:Const, val tv:Const, val indices:List[String]) extends Const {
  override def toString = ("insertvalue (" + vect + ", " + tv + listToString(", ", indices, ", ", "") + ")")
}
case class ConstExtractElement(val tv:Const, val index:Const) extends Const {
  override def toString = ("extractelement (" + tv + ", " + index + ")")
}

// this is an extension of imir, it requires a modification of llvm-as to load
// this and convert it to integer constant
// we use zero at this for testing


//trait MetaValue extends Value
abstract class MetaConst extends Const //with MetaValue
case class MDConst(val c:Const) extends MetaConst {
  override def toString = "!" + c.toString
}
case class MDString(n:QuoteStr) extends MetaConst {
  override def toString = "!" + n.toString
}
case class MDNode(n:String) extends MetaConst {
  override def toString = "!" + n
}
case class MDVar(n:String) extends MetaConst {
  override def toString = "!" + n
}
case class MDRef(val n:LocalVarOrID) extends MetaConst {
  override def toString = n.toString
}

sealed case class Label(val s:String) extends Emitter { override def toString = s + ":" }


sealed abstract class AggregateValue(val list:List[Value]) extends Value {
  def uses = list.flatMap(_.uses)
}
case class StructValue(val fields:List[Value], isPacked:Boolean = false) extends AggregateValue(fields)  {
  override def toString =
    ((if (isPacked) "<{{" else "{{") + (fields mkString ", ") + (if (isPacked) "}}>" else "}}"))
}
case class VectorValue(val fields:List[Value]) extends AggregateValue(fields) {
  override def toString = ("<<" + (fields mkString ", ") + ">>")
}
case class ArrayValue(val elems:List[Value]) extends AggregateValue(elems) {
  override def toString = ("[[" + (elems mkString ", ") + "]]")
}
// RHS
abstract class RHS() extends Value

// RHS- Arithmaitc Expression etc
sealed abstract class Expr() extends RHS
case class ArithmaticExpr(val binOp:ArithmaticOp, val carry:List[Carry], val t:Type, val v1:Value, val v2:Value)
  extends Expr {
  override def toString = (binOp + listToString(" ", carry, " ", "") + " " + t + " " + v1 + ", " + v2)
  def uses = v1.uses:::v2.uses
}
case class ConstExpr(val const:Const) extends Expr {
  override def toString = const.toString
  def uses = const.uses
}
case class ICmpExpr(val cmp:ICmpOp, val t:Type, val v1:Value, val v2:Value) extends Expr {
  override def toString = ("icmp " + cmp + " " + t + " " + v1 + ", " + v2)
  def uses = v1.uses:::v2.uses
}
case class FCmpExpr(val cmp:FCmpOp, val t:Type, v1:Value, val v2:Value) extends Expr {
  override def toString = ("fcmp " + cmp + " " + t + " " + v1 + ", " + v2)
  def uses = v1.uses:::v2.uses
}
case class BitwiseExpr(val binOp:BitwiseOp, val t:Type, val v1:Value, val v2:Value) extends Expr {
  override def toString = (binOp + " " + t + " " + v1 + ", " + v2)
  def uses = v1.uses:::v2.uses
}
case class GetElemPtr(val tv:Value, val indices:List[Const]) extends Expr {
  override def toString = ("getelementptr " + tv + listToString (", ", indices, ", ", ""))
  def uses = tv.uses:::(indices.flatMap(_.uses))
}
case class GetResult(val tv:Value, val index:String) extends Expr { // remove in 3.0
  override def toString = ("getresult " + tv + ", " + index)
  def uses = tv.uses
}
case class CastExpr(val op:ConvOp, val tv:Value, val t:Type) extends Expr {
  override def toString = (op + " " + tv + " to " + t)
  def uses = tv.uses
}
case class Select (val c:Value, val t:Value, val v:Value) extends Expr {
  override def toString = ("select " + c + ", " + t + ", " + v)
  def uses = c.uses:::t.uses:::v.uses
}
case class Extract (val tv:Value, val idx:Int) extends Expr {
  override def toString = "extract " + tv + ", " + idx
  def uses = tv.uses
}

// RHS - Memory operations
sealed abstract class MemOp() extends RHS
case class Alloca(val ty:Type, val size:Option[Value], val align:Option[Align]) extends MemOp {
  override def toString = ("alloca " + ty + optToString(", ", size) + optToString (", ", align))
  def uses = size.foldLeft(List[Use]())((p,x) => p:::x.uses)
}
case class Malloc(val ty:Type, val size:Option[Value], val align:Option[Align]) extends MemOp {
  override def toString = ("malloc " + ty + optToString (", ", size) + optToString (", ", align))
  def uses = size.foldLeft(List[Use]())((p,x) => p:::x.uses)
}
case class Free(val tv:Value) extends MemOp {
  override def toString = ("free " + tv)
  def uses = tv.uses
}
case class Load(val volatile:Boolean, val ptr:Value, val align:Option[Align]) extends MemOp {
  override def toString = ((if (volatile) "volatile " else "") + "load " + ptr + optToString (", ", align))
  def uses = ptr.uses
}
case class Store(val volatile:Boolean, val v:Value, val ptr:Value, val align:Option[Align]) extends MemOp {
  override def toString = ((if (volatile) "volatile " else "") + "store " + v + ", " + ptr + optToString (", ", align))
  def uses = v.uses:::ptr.uses
}

// Inline Assembly
case class InlineAsm(val hasSideEffect:Boolean, val alignStack:Boolean, val s1:QuoteStr, val s2:QuoteStr)
  extends Value {
  def uses = List()
  override def toString = "asm " + (if (hasSideEffect) "sideeffect " else "") +
    (if (alignStack) "alignstack " else "") + s1 + ", "  + s2
}


// RHS other expression
case class Phi(val ty:Type, val ins:List[(Value, Value)]) extends RHS {
  override def toString =
    ("phi " + ty + " " +
      ins.tail.foldLeft("[" + ins.head._1 + "," + ins.head._2 + "]") ((p, e) => p + ", " + "[" + e._1 + ", " + e._2 + "]"))

  def uses = ins.flatMap(x=> x._1.uses:::x._2.uses)
}

case class Call(val cconv:Option[CConv], val retAttrs:List[Attr], val retType:Type, val fn:VarOrID, val actualParams:ActualParamList,
                val funAttrs:List[Attr]) extends RHS {
  override def toString =
    ("call" + optToString (" ", cconv) + listToString (" ", retAttrs, " ", "") + " " +
      retType + " " + fn + actualParams + listToString(" ", funAttrs, " ", ""))

  def uses = fn.uses:::actualParams.uses
}

case class ExtractElement(val tv:Value, val index:Value) extends RHS  {
  override def toString = ("extractelement " + tv + ", " + index)
  def uses = tv.uses:::index.uses
}
case class InsertElement(val vect:Value, val tv:Value, val index:Value)
  extends RHS {
  override def toString = ("insertelement " + vect + ", " + tv + ", " + index)
  def uses = vect.uses:::tv.uses:::index.uses
}
case class ShuffleVector(val vect1:Value, val vect2:Value, val mask:Value)
  extends RHS {
  override def toString = ("shufflevector " + vect1 + ", " + vect2 + ", " + mask)
  def uses = vect1.uses:::vect2.uses:::mask.uses
}
case class ExtractValue(val tv:Value, val indices:List[String])
  extends RHS {
  override def toString = ("extractvalue " + tv + listToString (", ", indices, ", ", ""))
  def uses = tv.uses
}
case class InsertValue(val vect:Value, val tv:Value, val indices:List[String])
  extends RHS {
  override def toString = ("insertvalue " + vect + ", " + tv + listToString (", ", indices, ", ", ""))
  def uses = vect.uses:::tv.uses
}

case class VaArg(val tv:Value, val v:Type) extends RHS {
  override def toString = "va_arg " + tv + ", " + v
  def uses = tv.uses
}


case class InstSeq[T](val list:List[T]) {
  override def toString() = (list mkString "\n")
}

sealed abstract class AbsInst extends Value with Def with Emitter {
}

// Instruction
case class Instruction(val lhs:Option[VarOrID], val rhs:RHS, val dbg:List[Dbg]=List()) extends AbsInst {
  override def toString = ("  " + optToString(lhs, " = ") + rhs.toString +
    (if (dbg.isEmpty) "" else (dbg.mkString (", ", ",", ""))))
  def uses = rhs.uses
  def defined = lhs
}

case class Comment(s:Any) extends AbsInst {
  override def toString = "; " + s
  def uses = List()
  def defined = None
}

// Terminator Instruction
sealed abstract class ControlInst extends Use with Emitter
case class Unreachable() extends ControlInst {
  override def toString = "unreachable"
  def uses = List()
}

case class Ret (val v:List[Value]) extends ControlInst {
  override def toString = (if (v.isEmpty) "ret void" else "ret" + listToString (" ", v, ", ", ""))

  def uses = v.flatMap(_.uses)
}
case class Br(val l:Value) extends ControlInst {
  override def toString = ("br " + l)
  def uses = l.uses
}
case class CBr(val cond:Value, val tru:Value, val fal:Value) extends ControlInst {
  override def toString = ("br " + cond + ", " + tru + "," + fal)
  def uses = cond.uses:::tru.uses:::fal.uses
}
case class IndirectBr(val cond:Value, val branchs:List[Value]) extends ControlInst {
  override def toString = ("indirectbr " + cond + ", " + "[" + listToString ("", branchs, ", ", "") + "]")
  def uses = cond.uses:::branchs.flatMap(_.uses)
}
case class Switch(val v:Value, val default:Value, val jumptable:List[(Value, Value)]) extends ControlInst {
  override def toString = ("switch " + v + "," + default + "[" +
    jumptable.foldLeft("")((p, e) => p + "\n" + e._1 + "," + e._2) + "]")

  def uses = v.uses:::default.uses:::jumptable.flatMap(x=> x._1.uses:::x._2.uses)
}
case class Invoke(val lhs:Option[VarOrID], val cconv:Option[CConv], val retAttrs:List[Attr],
                  val fn:Value, val actualParams:ActualParamList, val funAttrs:List[Attr],
                  val label:Value, val exception:Value) extends ControlInst with Def with Value {
  override def toString =
    (optToString(lhs, " = " ) +  "invoke" + optToString (" ", cconv) +
      listToString (" ", retAttrs, " ", " ") + fn + actualParams + listToString (" ", funAttrs, " ", "") +
      " " + "to " + label + " unwind " + exception)

  def uses = fn.uses:::actualParams.uses:::label.uses:::exception.uses
  def defined = lhs
}

case class Unwind() extends ControlInst {
  override def toString = ("unwind")
  def uses = List()
}

case class ControlInstDbg(val inst:ControlInst, val dbg:List[Dbg]) extends Use {
  override def toString = "  " + inst + (if (dbg.isEmpty) "" else (dbg.mkString (", ", ",", "")))
  def uses = inst.uses
}

case class ActualParam(val attrs1:List[Attr], val align:Option[Align], val v:Value,
                       val attrs2:List[Attr])
  extends Use with Emitter {
  override def toString =
    (listToString (" ", attrs1, " ", "") + " " + optToString (align, " ") + v + listToString (" ", attrs2, " ", ""))
  def uses = v.uses
}

case class ActualParamList(val list:List[ActualParam]) extends Use with Emitter {
  override def toString = ("(" + listToString ("", list, ", ", "") + ")")
  def uses = list.flatMap(_.uses)
  def values = list.map(_.v)
}

case class Dbg(val s:String, val meta:MetaConst) {
  override def toString = "!" + s + "  " + meta
}


// Top level element in a module
trait TopLevel extends Emitter

// Alias
case class Alias(val lhs:Option[GlobalVarOrID], val visibility:Option[Visibility], val linkage:Option[Linkage],
                 val aliasee:Aliasee) extends TopLevel {
  override def toString =
    (optToString(lhs, " = ") + optToString (visibility, " ") +
      "alias " + optToString (linkage, " ") + aliasee.toString())
}

sealed abstract class Aliasee
case class AliaseeTV (tv:Value) extends Aliasee  { override def toString () = tv.toString }
case class AliaseeBitCast (tv:Value, t:Type) extends Aliasee {
  override def toString = "bitcast(" + tv + " to " + t + ")"
}
case class AliaseeGetElemPtr(get:ConstGetElemPtr) extends Aliasee { override def toString = get.toString() }

// Target
case class Target(val k:String, val v:QuoteStr) extends TopLevel {
  override def toString = "target " + k + " = " + v
}
case class Declare(val header:FunctionHeader) extends TopLevel {
  override def toString = ("declare " + header)
}
case class TypeDef(val name:String, val ty:Type) extends TopLevel {
  override def toString = name + " = type " + ty
}
case class UnamedType(val id:Int, val ty:Type) extends TopLevel {
  override def toString = "type " + ty + "; " + id
}

case class DepLibs(val l:List[QuoteStr]) extends TopLevel {
  override def toString = "deplibs = " + "[" + listToString ("", l, ", ", "") + "]"
}

case class Global(val lhs:Option[GlobalVarOrID], val linkage:Option[Linkage], val visiblilty:Option[Visibility],
                  val threadLocation:Boolean, val addrSpace:Option[AddrSpace], val globalType:GlobalType, val ty:Type,
                  val const:Option[Const], val section:Option[Section], val align:Option[Align]) extends TopLevel {
  override def toString =
    (optToString (lhs, " = ") + optToString (linkage, " ") + optToString (visiblilty, " ") +
      optToString (addrSpace, " ") + globalType + " " + ty + optToString (" ", const) +
      optToString(", ", section) + optToString (", ", align))
}

case class ModuleAsm(val str:QuoteStr) extends TopLevel { override def toString = "module asm " + str }
case class DbgInit(val lhs:String, val start:Int) extends TopLevel
case class StandardaloneMD(val lhs:String, val ty:Value) extends TopLevel { override def toString = "!" + lhs + " = " + ty}
case class NamedMD(val lhs:MDVar, val rhs:List[MDNode]) extends TopLevel {
  override def toString = lhs + " = " + "!{" + (rhs mkString ", ") + "}"
}

// Function definition
case class FunctionDef (val env:ImEnv, val funProto:FunctionHeader, val graph:Graph[AbsNode,O,C]) extends TopLevel {
  override def toString = "define " + funProto + " {\n" + graph.toInstStr + "\n}"
}

case class FunctionHeader (val linkage:Option[Linkage], val visibility:Option[Visibility],
                           val cconv:Option[CConv], val retAttrs:List[Attr],
                           val name:GlobalVarOrID, val funType:FunType,
                           val section:Option[Section], val align:Option[Align],
                           val gc:Option[GC])
  extends Emitter {
  override def toString =
    (optToString(linkage, " ") + optToString(visibility, " ") + optToString(cconv, " ") +
      listToString (" ", retAttrs, " ", " ") + funType.composeString(name.toString) +
      optToString (" ", section) + optToString (" ", align) + optToString (" ", gc))
}

sealed abstract class AbsNode() extends NonLocal
final case class FirstNode(val label:String, val entry:Int) extends AbsNode {
  def blockId() = entry
  def succBlockIds() = throw new Exception()
  override def toString = label + ":"
}
final case class StartNode(val label:String, val entry:Int) extends AbsNode {
  def blockId() = entry
  def succBlockIds() = throw new Exception()
  override def toString = label + ":"
}
final case class MiddleNode(val inst:AbsInst) extends AbsNode {
  def blockId() = throw new Exception()
  def succBlockIds() = throw new Exception()
  override def toString = inst.toString.replace('"', '_')

}
final case class LastNode(val targets:List[Int], val inst:ControlInst) extends AbsNode {
  def blockId() = throw new Exception()
  def succBlockIds() = targets
  override def toString = " " + inst.toString.replace('"', '_')
}

// Module
case class Module(val list:List[TopLevel], val defs:ImEnv) {
  override def toString = (list mkString "\n")
  def xList[X]:List[X] = list.filter(_.isInstanceOf[X]).map(_.asInstanceOf[X])

  def functionDefs:List[FunctionDef] =
    list.filter(_.isInstanceOf[FunctionDef]).map(_.asInstanceOf[FunctionDef])
}