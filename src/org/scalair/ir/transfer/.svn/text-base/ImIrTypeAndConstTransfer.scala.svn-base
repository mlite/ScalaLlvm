package org.scalair.ir.transfer

import org.scalair.ir.imir._

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


trait ImIrTypeAndConstTransfer[F] extends ImIrVarOrIdTransfer[F] { self =>
  import ET._
  val vis: TypeAndConstVisitor[F]

  def clFunType(env:F)(x: ET[FunType]):F = if (vis.visitFunType(env)(x)) {
    vis.bcFunType(env)(x)
    clType(env)(asA(x, asT(x).retType))
    clFormalParamList(env)(asA(x, asT(x).formalParams))
    vis.acFunType(env)(x)
  } else env

  def clType(env:F)(x:ET[Type]):F = if (vis.visitType(env)(x)) {
    vis.bcType(env)(x)
    asT(x) match {
      case MetadataType() => ()
      case OpaqueType() => ()
      case TypeName(n) => ()
      case TypeNo(no) => ()
      case TypeUpRef(no) => ()
      case ArrayType(n, b) => clType(env)(asA(x,b))
      case VectorType(n, b) => clType(env)(asA(x,b))
      case StructType(e, p) => for (et <- e) clType(env)(asA(x,et))
      case PointerType(a, b) => clType(env)(asA(x,a))
      case v: FunType => clFunType(env)(asA(x,v))
      case _ => env
    }
    vis.acType(env)(x)
  } else env



  def clMetaConst(env:F)(x: ET[MetaConst]):F = asT(x) match {
    case v:MDConst => clMDConst(env)(asA(x,v))
    case v:MDNode =>  clMDNode(env)(asA(x,v))
    case v:MDString => clMDString(env)(asA(x,v))
    case v:MDVar => clMDVar(env)(asA(x,v))
    case v:MDRef => clMDRef(env)(asA(x,v))
  }


  def clMDConst(env:F)(x:ET[MDConst]):F = if (vis.visitMDConst(env)(x)) {
    vis.bcMDConst(env)(x)
    clConst(env)(asA(x, asT(x).c))
    vis.acMDConst(env)(x)
  } else env

  def clMDNode(env:F)(x:ET[MDNode]):F = if (vis.visitMDNode(env)(x)) {
    vis.bcMDNode(env)(x)
    vis.acMDNode(env)(x)
  } else env

  def clMDVar(env:F)(x:ET[MDVar]):F = if (vis.visitMDVar(env)(x)) {
    vis.bcMDVar(env)(x)
    vis.acMDVar(env)(x)
  } else env

  def clMDString(env:F)(x:ET[MDString]):F = if (vis.visitMDString(env)(x)) {
    vis.bcMDString(env)(x)
    vis.acMDString(env)(x)
  } else env

  def clMDRef(env:F)(x:ET[MDRef]):F = if (vis.visitMDRef(env)(x)) {
    vis.bcMDRef(env)(x)
    clVarOrID(env)(asA(x, asT(x).n))
    vis.acMDRef(env)(x)
  } else env

  def clAtomicConst(env:F)(x: ET[ConstLiteral]):F = if (vis.visitAtomicConst(env)(x)) {
    vis.bcAtomicConst(env)(x)
    asT(x) match {
      case SizeOf(t) => clType(env)(asA(x,t))
      case _ => env
    }
    vis.acAtomicConst(env)(x)
  } else env

  def clConstGetElemPtr(env:F)(x: ET[ConstGetElemPtr]):F = if (vis.visitConstGetElemPtr(env)(x)) {
    vis.bcConstGetElemPtr(env)(x)
    clConst(env)(asA(x, asT(x).base))
    for (i <- asT(x).indices) clConst(env)(asA(x,i))
    vis.acConstGetElemPtr(env)(x)
  } else env

  def clConst(env:F)(x: ET[Const]):F = if (vis.visitConst(env)(x)) {
    vis.bcConst(env)(x)
    asT(x) match {
      case GlobalAddr(g) => clGlobalVarOrID(env)(asA(x,g))
      case PrimitiveConst(_, v) => clAtomicConst(env)(asA(x,v))
      case v: MetaConst => clMetaConst(env)(asA(x,v))
      case StructConst(f, isPacked) => for (v <- f) clConst(env)(asA(x,v))
      case VectorConst(f) => for (v <- f) clConst(env)(asA(x,v))
      case ArrayConst(e) => for (v <- e) clConst(env)(asA(x,v))
      case StrConst(str) => ()
      case LabelQuoteStr(str) => ()
      case BlockAddress(a1, a2) => {
        clGlobalVar(env)(asA(x,a1))
        clLocalVar(env)(asA(x,a2))
      }
      case ConstArithmaticExpr(bop, c, tv, op1) => {
        clConst(env)(asA(x,tv));
        clConst(env)(asA(x,op1))
      }
      case ConstBitwiseExpr(bop, tv, op1) => {
        clConst(env)(asA(x,tv))
        clConst(env)(asA(x,op1))
      }
      case ConstCast(conv, tv, t) => {
        clConst(env)(asA(x,tv));
        clType(env)(asA(x,t))
      }
      case ConstGetElemPtr(isInbounds, base, l) => {
        clConst(env)(asA(x,base))
        for (v <- l) clConst(env)(asA(x,v))
      }
      case ConstSelect(cond, v1, v2) => {
        clConst(env)(asA(x,cond))
        clConst(env)(asA(x,v1))
        clConst(env)(asA(x,v2))
      }
      case ConstICmpExpr(cmp, tv, v) => {
        clConst(env)(asA(x,tv))
        clConst(env)(asA(x,v))
      }
      case ConstFCmpExpr(cmp, tv, v) => {
        clConst(env)(asA(x,tv))
        clConst(env)(asA(x,v))
      }
      case ConstExtract(tv, idx) => clConst(env)(asA(x,tv))
      case ConstInsertElement(vect, tv, index) => {
        clConst(env)(asA(x,vect));
        clConst(env)(asA(x,tv));
        clConst(env)(asA(x,index))
      }
      case ConstShuffleVector(vect1, vect2, mask) => {
        clConst(env)(asA(x,vect1));
        clConst(env)(asA(x,vect2));
        clConst(env)(asA(x,mask))
      }
      case ConstExtractValue(tv, indices) => clConst(env)(asA(x,tv))
      case ConstInsertValue(vect, tv, indices) => {
        clConst(env)(asA(x,vect));
        clConst(env)(asA(x,tv))
      }
      case ConstExtractElement(tv, index) => {
        clConst(env)(asA(x,tv));
        clConst(env)(asA(x,index))
      }
    }
    vis.acConst(env)(x)
  } else env


  def clLabel(env:F)(x: ET[Label]):F = if (vis.visitLabel(env)(x)) {
    vis.bcLabel(env)(x)
    vis.acLabel(env)(x)
  } else env

  def clFormalParam(env:F)(x: ET[FormalParam]):F = if (vis.visitFormalParam(env)(x)) {
    vis.bcFormalParam(env)(x)
    clType(env)(asA(x, asT(x).ty))
    for (id <- asT(x).id) clLocalVarOrID(env)(asA(x,id))
    vis.acFormalParam(env)(x)
  } else env

  def clFormalParamList(env:F)(x: ET[FormalParamList]):F = if (vis.visitFormalParamList(env)(x)) {
    vis.bcFormalParamList(env)(x)
    for (x1 <- asT(x).list) clFormalParam(env)(asA(x,x1))
    vis.acFormalParamList(env)(x)
  } else env

  def clFunctionHeader(env:F)(x: ET[FunctionHeader]):F = if (vis.visitFunctionHeader(env)(x)) {
    vis.bcFunctionHeader(env)(x)
    clGlobalVarOrID(env)(asA(x,asT(x).name))
    clFunType(env)(asA(x, asT(x).funType))
    vis.acFunctionHeader(env)(x)
  } else env


  trait TypeAndConstVisitor[F] extends VarOrIdVisitor[F] {
    def visitFunType(fact:F)(x: ET[FunType]): Boolean = true
    // decide whether to visit this node and its children
    def bcFunType(fact:F)(x: ET[FunType]):F = fact
    def acFunType(fact:F)(x: ET[FunType]):F = fact

    def visitType(fact:F)(x: ET[Type]):Boolean = true
    def bcType(fact:F)(x: ET[Type]):F = fact
    def acType(fact:F)(x: ET[Type]):F = fact

    def visitFormalParam(fact:F)(x:ET[FormalParam]):Boolean = true
    def bcFormalParam(fact:F)(x:ET[FormalParam]):F = fact
    def acFormalParam(fact:F)(x:ET[FormalParam]):F = fact

    def visitFormalParamList(fact:F)(x:ET[FormalParamList]):Boolean = true
    def bcFormalParamList(fact:F)(x:ET[FormalParamList]):F = fact
    def acFormalParamList(fact:F)(x:ET[FormalParamList]):F = fact

    def visitAtomicConst(fact:F)(x:ET[ConstLiteral]):Boolean = true
    def bcAtomicConst(fact:F)(x:ET[ConstLiteral]):F = fact
    def acAtomicConst(fact:F)(x:ET[ConstLiteral]):F = fact

    def visitConstGetElemPtr(fact:F)(x:ET[ConstGetElemPtr]):Boolean = true
    def bcConstGetElemPtr(fact:F)(x:ET[ConstGetElemPtr]):F = fact
    def acConstGetElemPtr(fact:F)(x:ET[ConstGetElemPtr]):F = fact

    def visitConst(fact:F)(x:ET[Const]):Boolean = true
    def bcConst(fact:F)(x:ET[Const]):F = fact
    def acConst(fact:F)(x:ET[Const]):F = fact

    // it's a constant local code address
    def visitLabel(fact:F)(x:ET[Label]): Boolean = true
    def bcLabel(fact:F)(x:ET[Label]):F = fact
    def acLabel(fact:F)(x:ET[Label]):F = fact

    def visitMDConst(fact:F)(x:ET[MDConst]): Boolean = true
    def bcMDConst(fact:F)(x:ET[MDConst]):F = fact
    def acMDConst(fact:F)(x:ET[MDConst]):F = fact

    def visitMDVar(fact:F)(x:ET[MDVar]): Boolean = true
    def bcMDVar(fact:F)(x:ET[MDVar]):F = fact
    def acMDVar(fact:F)(x:ET[MDVar]):F = fact

    def visitMDNode(fact:F)(x:ET[MDNode]): Boolean = true
    def bcMDNode(fact:F)(x:ET[MDNode]):F = fact
    def acMDNode(fact:F)(x:ET[MDNode]):F = fact

    def visitMDRef(fact:F)(x:ET[MDRef]): Boolean = true
    def bcMDRef(fact:F)(x:ET[MDRef]):F = fact
    def acMDRef(fact:F)(x:ET[MDRef]):F = fact

    def visitMDString(fact:F)(x:ET[MDString]): Boolean = true
    def bcMDString(fact:F)(x:ET[MDString]):F = fact
    def acMDString(fact:F)(x:ET[MDString]):F = fact

    // yes this is a compile time constant
    // it's a constant global code address
    def visitFunctionHeader(fact:F)(x: ET[FunctionHeader]): Boolean = true
    def bcFunctionHeader(fact:F)(x: ET[FunctionHeader]):F = fact
    def acFunctionHeader(fact:F)(x: ET[FunctionHeader]):F = fact
  }
}