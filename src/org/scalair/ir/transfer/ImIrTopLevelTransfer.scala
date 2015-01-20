package org.scalair.ir.transfer

import org.scalair.ir.imir._

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


trait ImIrTopLevelTransfer[F] extends ImIrTypeAndConstTransfer[F] { self =>
  import ET._
  val vis: TopLevelVisitor[F]

  def clValue(fact:F)(x: ET[Value]):F = {
    asT(x) match {
      case v:VarOrID => clVarOrID(fact)(asA(x,v))
      case v:Const => clConst(fact)(asA(x,v))
      case v:InlineAsm => fact
    }
  }

  def clTopLevel(fact:F)(x: ET[TopLevel]):F = {
    asT(x) match {
      case Target(k, v) => fact
      case y:Declare => clDeclare(fact)(asA(x, y))
      case TypeDef(name, ty) => clType(fact)(asA(x, ty))
      case UnamedType(id, ty) => clType(fact)(asA(x, ty))
      case DepLibs(l) => fact
      case ModuleAsm(str) => fact
      case v:Alias => clAlias(fact)(asA(x, v))
      case v:Global => clGlobal(fact)(asA(x,v))
      case v:StandardaloneMD => clStandardaloneMD(fact)(asA(x, v))
      case v:NamedMD => clNamedMD(fact)(asA(x,v))
      case v:DbgInit => clDbgInit(fact)(asA(x,v))
      case v:FunctionDef => fact /*do nothing*/
    }
  }

  def clDeclare(fact:F)(x:ET[Declare]):F = {
    if (vis.visitDeclare(fact)(x)) {
      vis.bcDeclare(fact)(x)
      clFunctionHeader(fact)(asA(x, asT(x).header))
      vis.acDeclare(fact)(x)
    } else fact
  }

  def clAliasee(fact:F)(x:ET[Aliasee]):F = {
    if (vis.visitAliasee(fact)(x)) {
      vis.bcAliasee(fact)(x)
      asT(x) match {
        case AliaseeTV(tv) => clValue(fact)(asA(x, tv))
        case AliaseeBitCast(tv, t) => {
          clValue(fact)(asA(x,tv))
          clType(fact)(asA(x,t))
        }
        case AliaseeGetElemPtr(get) => {
          clConstGetElemPtr(fact)(asA(x,get))
        }
      }
      vis.acAliasee(fact)(x)
    } else fact
  }

  def clAlias(fact:F)(x:ET[Alias]):F = {
    if (vis.visitAlias(fact)(x)) {
      vis.bcAlias(fact)(x)
      for (g <- asT(x).lhs) clGlobalVarOrID(fact)(asA(x,g))
      clAliasee(fact)(asA(x, asT(x).aliasee))
      vis.acAlias(fact)(x)
    } else fact
  }

  def clGlobal(fact:F)(x:ET[Global]):F = {
    if (vis.visitGlobal(fact)(x)) {
      for (g <- asT(x).lhs) clGlobalVarOrID(fact)(asA(x,g))
      clType(fact)(asA(x,asT(x).ty))
    } else fact
  }

  def clStandardaloneMD(fact:F)(x:ET[StandardaloneMD]):F = {
    if (vis.visitStandardaloneMD(fact)(x)) {
      vis.bcStandardaloneMD(fact)(x)
      clValue(fact)(asA(x,asT(x).ty))
      vis.acStandardaloneMD(fact)(x)
    } else fact
  }

  def clNamedMD(fact:F)(x:ET[NamedMD]):F = {
    if (vis.visitNamedMD(fact)(x)) {
      vis.bcNamedMD(fact)(x)
      clMDVar(fact)(asA(x,asT(x).lhs))
      asT(x).rhs.foldLeft(fact) { (p, x1) => clMDNode(p)(asA(x,x1)) }
      vis.acNamedMD(fact)(x)
    } else fact
  }

  def clDbgInit(fact:F)(x:ET[DbgInit]):F = fact


  def clFunctionDef(fact:F)(x:ET[FunctionDef]):F = if (vis.visitFunctionDef(fact)(x)) {
    vis.bcFunctionDef(fact)(x)
    clFunctionHeader(fact)(asA(x,asT(x).funProto))
    vis.acFunctionDef(fact)(x)
  } else fact



  trait TopLevelVisitor[F] extends TypeAndConstVisitor[F]  {

    def visitValue(fact:F)(x:ET[Value]):Boolean = true
    def bcValue(fact:F)(x:ET[Value]):F = fact
    def acValue(fact:F)(x:ET[Value]):F = fact

    def visitDeclare(fact:F)(x:ET[Declare]):Boolean = true
    def bcDeclare(fact:F)(x:ET[Declare]):F = fact
    def acDeclare(fact:F)(x:ET[Declare]):F = fact

    def visitAliasee(fact:F)(x:ET[Aliasee]):Boolean = true
    def bcAliasee(fact:F)(x:ET[Aliasee]):F = fact
    def acAliasee(fact:F)(x:ET[Aliasee]):F = fact

    def visitAlias(fact:F)(x:ET[Alias]):Boolean = true
    def bcAlias(fact:F)(x:ET[Alias]):F = fact
    def acAlias(fact:F)(x:ET[Alias]):F = fact

    def visitGlobal(fact:F)(x:ET[Global]):Boolean = true
    def bcGlobal(fact:F)(x:ET[Global]):F = fact
    def acGlobal(fact:F)(x:ET[Global]):F = fact

    def visitStandardaloneMD(fact:F)(x:ET[StandardaloneMD]):Boolean = true
    def bcStandardaloneMD(fact:F)(x:ET[StandardaloneMD]):F = fact
    def acStandardaloneMD(fact:F)(x:ET[StandardaloneMD]):F = fact

    def visitNamedMD(fact:F)(x:ET[NamedMD]):Boolean = true
    def bcNamedMD(fact:F)(x:ET[NamedMD]):F = fact
    def acNamedMD(fact:F)(x:ET[NamedMD]):F = fact

    def visitDbgInit(fact:F)(x:ET[DbgInit]):Boolean = true
    def bcDbgInit(fact:F)(x:ET[DbgInit]):F = fact
    def acDbgInit(fact:F)(x:ET[DbgInit]):F = fact

    def visitFunctionDef(fact:F)(x:ET[FunctionDef]):Boolean = true
    def bcFunctionDef(fact:F)(x:ET[FunctionDef]):F = fact
    def acFunctionDef(fact:F)(x:ET[FunctionDef]):F = fact
  }
}