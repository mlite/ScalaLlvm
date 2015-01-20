package org.scalair.ir.rewriter

import org.scalair.ir.imir._

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


trait ImIrTopLevelRewriter[S,F] extends ImIrValueRewriter[S,F] { self =>

  def rwTopLevel(fact:F)(x: A[TopLevel]):A[TopLevel] = x flatMap {
    _ match {
      case Left(e:Target) => x
      case Left(e:Declare) => rwDeclare(fact)(left(e))
      case Left(e:TypeDef) => rwTypeDef(fact)(left(e))
      case Left(e:UnamedType) => rwUnamedType(fact)(left(e))
      case Left(e:DepLibs) => x
      case Left(e:ModuleAsm) => x
      case Left(e:Alias) => rwAlias(fact)(left(e))
      case Left(e:Global) => rwGlobal(fact)(left(e))
      case Left(e:StandardaloneMD) => rwStandardaloneMD(fact)(left(e))
      case Left(e:NamedMD) => rwNamedMD(fact)(left(e))
      case Left(e:DbgInit) => rwDbgInit(fact)(left(e))
      case Left(e:FunctionDef) => x
    }
  }

  def rwTypeDef(env:F)(x:A[TypeDef]):A[TypeDef] = x
  def rwUnamedType(env:F)(x:A[UnamedType]):A[UnamedType] = x
  def rwDeclare(env:F)(x:A[Declare]):A[Declare] = x
  def rwAliasee(env:F)(x:A[Aliasee]):A[Aliasee] = x
  def rwAlias(env:F)(x:A[Alias]):A[Alias] = x
  def rwGlobal(env:F)(x:A[Global]):A[Global] = x
  def rwStandardaloneMD(env:F)(x:A[StandardaloneMD]):A[StandardaloneMD] = x
  def rwNamedMD(env:F)(x:A[NamedMD]):A[NamedMD] = x
  def rwDbgInit(env:F)(x:A[DbgInit]):A[DbgInit] = x
  def rwFunctionDef(env:F)(x:A[FunctionDef]):A[FunctionDef] = x
}