package org.scalair.ir.rewriter

import org.scalair.ir.imir._

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


trait ImIrTypeAndConstRewriter[S,F] extends ImIrVarOrIdRewriter[S,F] {

  def rwMetadataType(fact:F)(x:A[MetadataType]):A[Type] = x
  def rwOpaqueType(fact:F)(x:A[OpaqueType]):A[Type] = x
  def rwTypeName(fact:F)(x:A[TypeName]):A[Type] = x
  def rwTypeNo(fact:F)(x:A[TypeNo]):A[Type] = x
  def rwTypeUpRef(fact:F)(x:A[TypeUpRef]):A[Type] = x
  def rwArrayType(fact:F)(x:A[ArrayType]):A[Type] = x
  def rwVectorType(fact:F)(x:A[VectorType]):A[Type] = x
  def rwStructType(fact:F)(x:A[StructType]):A[Type] = x
  def rwPointerType(fact:F)(x:A[PointerType]):A[Type] = x
  def rwFunType(env:F)(x:A[FunType]):A[Type] = x

  def rwType(env:F)(x:A[Type]):A[Type] = x flatMap {
    _ match {
      case Left(v:MetadataType) => rwMetadataType(env)(left(v))
      case Left(v:OpaqueType) => rwOpaqueType(env)(left(v))
      case Left(v:TypeName) => rwTypeName(env)(left(v))
      case Left(v:TypeNo) => rwTypeNo(env)(left(v))
      case Left(v:TypeUpRef) => rwTypeUpRef(env)(left(v))
      case Left(v:ArrayType) => rwArrayType(env)(left(v))
      case Left(v:VectorType) => rwVectorType(env)(left(v))
      case Left(v:StructType) => rwStructType(env)(left(v))
      case Left(v:PointerType) => rwPointerType(env)(left(v))
      case Left(v:FunType) => rwFunType(env)(left(v))
      case _ => throw new Exception()
    }
  }

  def rwMetaConst(env:F)(x: A[MetaConst]):A[MetaConst] = x flatMap {
    _ match {
      case Left(v:MDConst) => rwMDConst(env)(left(v))
      case Left(v:MDNode) =>  rwMDNode(env)(left(v))
      case Left(v:MDString) => rwMDString(env)(left(v))
      case Left(v:MDVar) => rwMDVar(env)(left(v))
      case Left(v:MDRef) => rwMDRef(env)(left(v))
    }
  }


  def rwMDConst(env:F)(x:A[MDConst]):A[MetaConst] = x
  def rwMDNode(env:F)(x:A[MDNode]):A[MetaConst] = x
  def rwMDVar(env:F)(x:A[MDVar]):A[MetaConst] = x
  def rwMDString(env:F)(x:A[MDString]):A[MetaConst] = x
  def rwMDRef(env:F)(x:A[MDRef]):A[MetaConst] = x
  def rwAtomicConst(env:F)(x: A[ConstLiteral]):A[ConstLiteral] = x
  def rwConstGetElemPtr(env:F)(x: A[ConstGetElemPtr]):A[Const] = x
  def rwStructConst(env:F)(x:A[StructConst]):A[Const] = x
  def rwVectorConst(env:F)(x:A[VectorConst]):A[Const] = x
  def rwArrayConst(env:F)(x:A[ArrayConst]):A[Const] = x
  def rwStrConst(env:F)(x:A[StrConst]):A[Const] = x
  def rwLabelQuoteStr(env:F)(x:A[LabelQuoteStr]):A[Const] = x
  def rwBlockAddress(env:F)(x:A[BlockAddress]):A[BlockAddress] = x
  def rwConstLiteral(env:F)(x:A[ConstLiteral]):A[ConstLiteral] = x
  def rwConstArithmaticExpr(env:F)(x:A[ConstArithmaticExpr]):A[Const] = x
  def rwConstBitwiseExpr(env:F)(x:A[ConstBitwiseExpr]):A[Const] = x
  def rwConstCast(env:F)(x:A[ConstCast]):A[Const] = x
  def rwConstSelect(env:F)(x:A[ConstSelect]):A[Const] = x
  def rwConstICmpExpr(env:F)(x:A[ConstICmpExpr]):A[Const] = x
  def rwConstFCmpExpr(env:F)(x:A[ConstFCmpExpr]):A[Const] = x
  def rwConstExtract(env:F)(x:A[ConstExtract]):A[Const] = x
  def rwConstInsertElement(env:F)(x:A[ConstInsertElement]):A[Const] = x
  def rwConstShuffleVector(env:F)(x:A[ConstShuffleVector]):A[Const] = x
  def rwConstExtractValue(env:F)(x:A[ConstExtractValue]):A[Const] = x
  def rwConstInsertValue(env:F)(x:A[ConstInsertValue]):A[Const] = x
  def rwConstExtractElement(env:F)(x:A[ConstExtractElement]):A[Const] = x
  def rwGlobalAddr(env:F)(x:A[GlobalAddr]):A[Const] = x
  def rwPrimitiveConst(env:F)(x:A[PrimitiveConst]):A[Const] = x flatMap {
    case Left(PrimitiveConst(t,c)) => {
      for {
        t1 <- rwType(env)(left(t))
        c1 <- rwConstLiteral(env)(left(c))
      } yield {
        (t1,c1) match {
          case (Left(t2), Left(c2)) => Left(PrimitiveConst(t2,c2))
          case (Left(t2), Right(c2)) => Right(PrimitiveConst(t2,c2))
          case (Right(t2), Left(c2)) => Right(PrimitiveConst(t2,c2))
          case (Right(t2), Right(c2)) => Right(PrimitiveConst(t2,c2))
        }
      }
    }
  }

  def rwConst(env:F)(x: A[Const]):A[Const] = x flatMap {
    _ match {
      case Left(v:GlobalAddr) => rwGlobalAddr(env)(left(v))
      case Left(v:PrimitiveConst) => rwPrimitiveConst(env)(left(v))
      case Left(v:MetaConst) => rwMetaConst(env)(left(v))
      case Left(v:StructConst) => rwStructConst(env)(left(v))
      case Left(v:VectorConst) => rwVectorConst(env)(left(v))
      case Left(v:ArrayConst) => rwArrayConst(env)(left(v))
      case Left(v:StrConst) => rwStrConst(env)(left(v))
      case Left(v:LabelQuoteStr) => rwLabelQuoteStr(env)(left(v))
      case Left(v:BlockAddress) => rwBlockAddress(env)(left(v))
      case Left(v:ConstArithmaticExpr) => rwConstArithmaticExpr(env)(left(v))
      case Left(v:ConstBitwiseExpr) => rwConstBitwiseExpr(env)(left(v))
      case Left(v:ConstCast) => rwConstCast(env)(left(v))
      case Left(v:ConstGetElemPtr) => rwConstGetElemPtr(env)(left(v))
      case Left(v:ConstSelect) => rwConstSelect(env)(left(v))
      case Left(v:ConstICmpExpr) => rwConstICmpExpr(env)(left(v))
      case Left(v:ConstFCmpExpr) => rwConstFCmpExpr(env)(left(v))
      case Left(v:ConstExtract) => rwConstExtract(env)(left(v))
      case Left(v:ConstInsertElement) => rwConstInsertElement(env)(left(v))
      case Left(v:ConstShuffleVector) => rwConstShuffleVector(env)(left(v))
      case Left(v:ConstExtractValue) => rwConstExtractValue(env)(left(v))
      case Left(v:ConstInsertValue) => rwConstInsertValue(env)(left(v))
      case Left(v:ConstExtractElement) => rwConstExtractElement(env)(left(v))
    }
  }

  def rwLabel(env:F)(x: A[Label]):A[Label] = x
  def rwFormalParam(env:F)(x: A[FormalParam]):A[FormalParam] = x
  def rwFormalParamList(env:F)(x: A[FormalParamList]):A[FormalParamList] = x
  def rwFunctionHeader(env:F)(x: A[FunctionHeader]):A[FunctionHeader] = x
}