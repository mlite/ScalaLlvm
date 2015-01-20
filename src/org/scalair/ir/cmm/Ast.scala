package org.scalair.ir.cmm

/** 
 * User: wangn
 * Date: Feb 27, 2010
 */

class Ast
{
  type name = String;
  type conv = String;
  type hint = String;
  type reg = String;
  type target = String;
  type aliasSet = String;
  type size = Int;
  type align = Int;
  type aligned = Int;
  type in_alias = Int;
  type op = String;
  
  class region(i: Int, j: Int)

  abstract class ty
  case class TyAt(val t: ty, val r: region) extends ty
  case class BitsTy(val s: size) extends ty
  case class TypeSynonym(val n: Name) extends ty

  abstract class name_or_mem
  case class NameOrMemAt(val nameOrMem: name_or_mem, val region: region) extends name_or_mem
  case class Name(val hintOpt: Option[hint], val name: name, val alignedOpt: Option[aligned]) extends name_or_mem
  case class Mem(val ty: ty, val expr: pexpr, val alignedOpt: Option[aligned], val inAliasList: List[in_alias]) extends name_or_mem

  class actual(val hintOpt: Option[hint], val expr: pexpr, val alignedOpt: Option[aligned])

  // linkage time constant expression
  abstract class cexpr
  case class Sint(val s: String, val tyOpt: Option[ty]) extends cexpr
  case class Uint(val s: String, val tyOpt: Option[ty]) extends cexpr
  case class Float(val s: String, val tyOpt: Option[ty]) extends cexpr
  case class Char(val i: Int, val tyOpt: Option[ty]) extends cexpr
  case class CBinOp(val lhs:cexpr,  val op:op, val rhs:cexpr) extends cexpr
  case class CUnOp(val op:op, val rhs:cexpr) extends cexpr

  // primitive expression
  abstract class pexpr
  case class CExpr (val expr:cexpr) extends pexpr
  case class Fetch(val nameOrMem: name_or_mem) extends pexpr
  case class PrimOp(val name: Name, val actualList: List[actual]) extends pexpr

  // expression
  abstract class expr
  case class ExprAt(val expr: expr, val region: region) extends expr
  case class PExpr(val primExpr:pexpr) extends expr
  case class BinOp(val lhs:pexpr, val op: op, val rhs:pexpr) extends expr
  case class UnOp(val op:op, val pexpr:expr) extends expr

  
  class import_decl(val sOpt: Option[String], val name: Name)
  class export_decl(val name: Name, val sOpt: Option[String])

  abstract class variance
  case class Invariant() extends variance
  case class Invisible() extends variance
  case class Variant() extends variance

  class register (val variance:variance, val hintOpt:Option[hint], val ty:ty, val name:name, val regOpt:Option[reg])

  abstract class arch
  case class MemSize(val s:Int) extends arch
  case object ByteOrderBig extends arch
  case object ByteOrderLittle extends arch
  case class FloatRepr(val s:String) extends arch
  case class CharSet(val s:Int) extends arch
  case class WordSize(val i:Int) extends arch
  case class PointerSize(val i:Int) extends arch

  abstract class decl
  case class DeclAt(val decl:decl, val region:region) extends decl
  case class Import(val tyOpt:Option[ty], val importList:List[import_decl]) extends decl
  case class Export(val tyOpt:Option[ty], val importList:List[export_decl]) extends decl
  case class Const(val tyOpt:Option[ty], val name:name, val expr:cexpr) extends decl
  case class TypeDef(val tyOpt:Option[ty], val nameList:List[name]) extends decl
  case class Registers(val registerList:Option[register]) extends decl
  case class Pragma(val u:String)  extends decl
  case class Target(val archList:List[arch]) extends decl

  class bare_formal(val hintOpt:Option[hint], val variance:variance, val ty:ty, val name:name, val alignedOpt:Option[aligned])

  class formal(val region:region, val bareFormal:bare_formal)

  abstract class mem_size
  case class NoSize() extends mem_size
  case class DynSize() extends mem_size
  case class FixSize(val expr:cexpr) extends mem_size

  abstract class init
  case class InitAt (val init:init, val region:region) extends init
  case class InitExprs (val exprList:List[cexpr]) extends init
  case class InitStr (val s:String) extends init
  case class InitUStr (val s:String) extends init

  abstract class datum
  case class DatumAt(val datum:datum, val region:region) extends datum
  case class Label(val name:name) extends datum
  case class Align(val align:align) extends datum
  case class MemDecl(val ty:ty,val memSize:mem_size, val initOpt:Option[init]) extends datum

  class cformal(val region:region, val hintOpt:Option[hint], val name:name, val alignedOpt:Option[aligned])

  abstract class flow
  case class FlowAt(val flow:flow, val region:region) extends flow
  case class CutsTo(val nameList:List[name]) extends flow
  case class UnwindsTo(val nameList:List[name]) extends flow
  case class ReturnsTo(val nameList:List[name]) extends flow
  case class NeverReturns() extends flow
  case class Aborts() extends flow

  abstract class mem
  case class AliasAt(val mem:mem, val region:region) extends mem
  case class Reads(val nameList:List[name]) extends mem
  case class Writes(val nameList:List[name]) extends mem

  abstract class procann
  case class Flow(val flow:flow) extends procann
  case class Alias(val mem:mem) extends procann  

  abstract class altcont(val expr0:expr, val expr1:expr)

  abstract class range
  case class Point(val expr:expr) extends range
  case class Range(val expr0:cexpr, val expr1:cexpr) extends range

  class guarded(val exprOpt:Option[expr], val expr:expr)

  abstract class arm
  case class ArmAt (val arm:arm, val region:region) extends arm
  case class Case (val rangeList:List[range], val bodyList:List[body]) extends arm

  class asm
  
  abstract class stmt
  case class StmtAt(val stmt:stmt, val region:region) extends stmt
  case class IfStmt(val expr:expr, val thenBodyList:List[body], val elseBodyList:List[body]) extends stmt
  case class SwitchStmt(val rangeOpt:Option[range], val expr:pexpr, val armList:List[arm]) extends stmt
  case class LabelStmt(val name:name) extends stmt
  case class ContStmt(val name:name, val cformalList:List[cformal]) extends stmt
  case class SpanStmt(val expr0:cexpr, val expr1:cexpr, val bodyList:List[body]) extends stmt
  case class AssignStmt(val nameOrMemList:List[name_or_mem], val guardedList:List[guarded]) extends stmt
  case class CallStmt(val nameOrMemList:List[name_or_mem], val convOpt:Option[conv], expr:pexpr,
                      val actualList:List[actual], val targetList:List[target], val procannList:List[procann])
  case class PrimStmt(val nameOrMemList:List[name_or_mem], val convOpt:Option[conv], val name:name, val actualList:List[actual],
                      val flowList:List[flow]) extends stmt
  case class GotoStmt(val expr:cexpr, val targetList:List[target]) extends stmt
  case class JumpStmt(val convOpt:Option[conv], val expr:cexpr, val actuaList:List[actual], val targetList:List[target]) extends stmt
  case class CutStmt(val expr:cexpr, val actualList:List[actual], val flowList:List[flow]) extends stmt
  case class ReturnStmt(val convOpt:Option[conv], val altcontOpt:Option[altcont], val actualList:List[actual]) extends stmt
  case class EmptyStmt() extends stmt
  case class CommentStmt(val s:String) extends stmt
  case class LimitcheckStmt(val expr:expr, val exprOpt:Option[expr]) extends stmt
  case class AsmStmt(val sList:List[String], val asmOpt:Option[asm]) extends stmt

  abstract class body
  case class BodyAt(val body:body, val region:region) extends body
  case class DeclBody(val decl:decl) extends body
  case class StmtBody(val stmt:stmt) extends body
  case class DataBody(val datumList:List[datum]) extends body

  class proc(val convOpt:Option[conv], val name:name, val formalList:List[formal], val bodyList:List[body], val region:region) {}

  abstract class section {}
  case class SectionAt(val section:section, val region:region) extends section
  case class Decl(val decl:decl) extends section
  case class Procedure(val proc:proc) extends section
  case class Datum(val datum:datum) extends section
  case class SSpan(val expr0:cexpr, val expr1:cexpr, val sectionList:List[section]) extends section

  abstract class toplevel {}
  case class ToplevelAt(val toplevel:toplevel, val region:region) extends toplevel
  case class Section(val name:name, val sectionList:List[section]) extends toplevel
  case class TopDecl(val decl:decl) extends toplevel
  case class TopProcedure(val proc:proc) extends toplevel

  class program (val topLevelList:List[toplevel])
}
