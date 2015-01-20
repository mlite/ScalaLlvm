package org.scalair.ir.rewriter

import org.scalair.hoopl._
import org.scalair.hoopl.Block
import org.scalair.ir.imir._
;

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


trait ImIrRewriter {
  type Inv // Invariants

  def rwFunType(inv:Inv)(x:FunType) = x

  def rwType(inv:Inv)(x:Type) = x

  def rwVarOrID(inv:Inv)(x:VarOrID) = x

  def rwGlobalVar(inv:Inv)(x:GlobalVar) = x

  def rwGlobalVarOrID(inv:Inv)(x:GlobalVarOrID) = x

  def rwLocalVar(inv:Inv)(x:LocalVar) = x

  def rwLocalVarOrID(inv:Inv)(x:LocalVarOrID) = x

  def rwMetaConst(inv:Inv)(x:MetaConst) = x

  def rwMDVar(inv:Inv)(x:MDVar) = x

  def rwMDNode(inv:Inv)(x:MDNode) = x

  def rwInlineAsm(inv:Inv)(x:InlineAsm) = x

  def rwConstLiteral(inv:Inv)(x:ConstLiteral) = x

  def rwConstGetElemPtr(inv:Inv)(x:ConstGetElemPtr) = x

  def rwConst(inv:Inv)(x:Const) = x

  def rwRHS(inv:Inv)(x:RHS) = x

  def rwExpr(inv:Inv)(x:Expr) = x

  def rwLabel(inv:Inv)(x:Label) = x

  def rwMemOp(inv:Inv)(x:MemOp) = x

  def rwValue(inv:Inv)(x:Value) = x

  def rwInstruction(inv:Inv)(x:AbsInst) = x

  def rwControlInst(inv:Inv)(x:ControlInst) = x

  def rwControlInstDbg(inv:Inv)(x:ControlInstDbg) = x

  def rwAbsNode(inv:Inv)(n:AbsNode) = n

  def rwBlock(inv:Inv)(x:Block[AbsNode,C,C]) = x

  def rwFormalParam(inv:Inv)(x:FormalParam) = x

  def rwFormalParamList(inv:Inv)(x:FormalParamList) = x

  def rwActualParam(inv:Inv)(x:ActualParam) = x

  def rwActualParamList(inv:Inv)(x:ActualParamList) = x

  def rwTopLevel(inv:Inv)(x:TopLevel) = x

  def rwDecare(inv:Inv)(x:Declare) = x

  def rwAliasee(inv:Inv)(x:Aliasee) = x

  def rwAlias(inv:Inv)(x:Alias) = x

  def rwGlobal(inv:Inv)(x:Global) = x

  def rwStandardaloneMD(inv:Inv)(x:StandardaloneMD) = x

  def rwNamedMD(inv:Inv)(x:NamedMD) = x

  def rwDbgInit(inv:Inv)(x:DbgInit) = x

  def rwFunctionHeader(inv:Inv)(x:FunctionHeader) = x

  def rwGraph(inv:Inv)(x:Graph[AbsNode,O,C]) = x

  def rwFunctionDef(inv:Inv)(x:FunctionDef) = x

  def rwDbg(inv:Inv)(x:Dbg) = x

  def rwModule(inv:Inv)(m:Module) = m
}
