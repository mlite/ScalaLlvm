package org.scalair.ir

import common.{Bitcast, Add}
import imir.AbsNode
import org.scalair.hoopl._

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


final case class ImIr2Llvm (m:imir.Module) {
  def clFunType(env:imir.ImEnv)(x:imir.FunType):llvm.FunType = x match {
    case imir.FunType(ret, fp, fa) => llvm.FunType(clType(env)(ret), clFormalParamList(env)(fp), fa)
  }

  def clType(env:imir.ImEnv)(x:imir.Type):llvm.Type = x match {
    case imir.I(i) => llvm.I(i)
    case imir.F(i) => llvm.F(i)
    case imir.V64() => llvm.V64()
    case imir.V128() => llvm.V128()
    case imir.VoidType() => llvm.VoidType()
    case imir.FloatType() => llvm.FloatType()
    case imir.DoubleType() => llvm.DoubleType()
    case imir.FP128Type() => llvm.FP128Type()
    case imir.X86FP80Type() => llvm.X86FP80Type()
    case imir.PPCFP128Type() => llvm.PPCFP128Type()
    case imir.X86MMXType() => llvm.X86MMXType()
    case imir.LabelType() => llvm.LabelType()
    case imir.MetadataType() => llvm.MetadataType()
    case imir.OpaqueType() => llvm.OpaqueType()
    case imir.TypeName(n) => llvm.TypeName(n)
    case imir.TypeNo(no) =>  llvm.TypeNo(no)
    case imir.TypeUpRef(no) => llvm.TypeUpRef(no)
    case imir.ArrayType(n, b) => llvm.ArrayType(n, clType(env)(b))
    case imir.VectorType(n, b) => llvm.VectorType(n, clType(env)(b))
    case imir.StructType(e, p) => llvm.StructType(e.map(clType(env)), p)
    case imir.PointerType(a, b) => llvm.PointerType(clType(env)(a), b)
    case imir.FunType(ret, fp, fa) => llvm.FunType(clType(env)(ret), clFormalParamList(env)(fp), fa)
  }


  def clValue(env:imir.ImEnv)(x:imir.Value):llvm.Value = x match {
    case x:imir.VarOrID => clVarOrID(env)(x)
    case x:imir.Const => clConst(env)(x)
    case x:imir.InlineAsm => clInlineAsm(env)(x)
  }

  def clVarOrID(env:imir.ImEnv)(x:imir.VarOrID) = x match {
    case x:imir.GlobalVarOrID => clGlobalVarOrID(env)(x)
    case x:imir.LocalVarOrID => clLocalVarOrID(env)(x)
  }

  def clGlobalVar(env:imir.ImEnv)(x:imir.GlobalVar) = x match {
    case imir.GlobalVar(s) => llvm.GlobalVar(s)
  }

  def clGlobalVarOrID(env:imir.ImEnv)(x:imir.GlobalVarOrID) = x match {
    case imir.GlobalID(n) => llvm.GlobalID(n.toString)
    case imir.GlobalVar(n) => llvm.GlobalVar(n)
  }

  def clLocalVar(env:imir.ImEnv)(x:imir.LocalVar) = x match {
    case imir.LocalVar(n) => llvm.LocalVar(n)
  }

  def clLocalVarOrID(env:imir.ImEnv)(x:imir.LocalVarOrID) = x match {
    case imir.LocalID(n) => llvm.LocalID(n.toString)
    case imir.LocalVar(n) => llvm.LocalVar(n)
    case imir.LocalStr(n) => llvm.LocalStr(n)
  } 

  def clMetaConst(env:imir.ImEnv)(x:imir.MetaConst) = x match {
    case imir.MDConst(c) => llvm.MDConst(clConst(env)(c))
    case imir.MDNode(n) => llvm.MDNode(n)
    case imir.MDString(n) => llvm.MDString(n)
    case imir.MDVar(n) => llvm.MDVar(n)
    case imir.MDRef(n) => llvm.MDRef(clLocalVarOrID(env)(n))
  }

  def clMDVar(env:imir.ImEnv)(x:imir.MDVar) = x match {
    case imir.MDVar(n) => llvm.MDVar(n)
  }

  def clMDNode(env:imir.ImEnv)(x:imir.MDNode) = x match {
    case imir.MDNode(n) => llvm.MDNode(n)
  }

  def clInlineAsm(env:imir.ImEnv)(x:imir.InlineAsm) = x match {
    case imir.InlineAsm(h, a, s1, s2) => llvm.InlineAsm(h, a, s1, s2)
  }

  def clAtomicConst(env:imir.ImEnv)(x:imir.ConstLiteral):llvm.AtomicConst = x match {
    case imir.IntConst(str) => llvm.IntConst(str)
    case imir.FloatConst(str) => llvm.FloatConst(str)
    case imir.NullValue() => llvm.NullValue()
    case imir.Undef() => llvm.Undef()
    case imir.BTrue() => llvm.BTrue()
    case imir.BFalse() => llvm.BFalse()
    case imir.Zero() => llvm.Zero()
    case imir.SizeOf(t) => llvm.IntConst(env.sizeofInByte(t).toString())
    case _ => throw new Exception("unexpected " + x)
  }

  def clConstGetElemPtr(env:imir.ImEnv)(x:imir.ConstGetElemPtr) = x match {
    case imir.ConstGetElemPtr(isInbounds, base, l) =>
      llvm.ConstGetElemPtr(isInbounds, clTC(env)(base), l.map(clTC(env)))
  }

  def clConst(env:imir.ImEnv)(x:imir.Const):llvm.Const = x match {
    case imir.GlobalAddr(g) => llvm.GlobalAddr(clGlobalVarOrID(env)(g))
    case imir.PrimitiveConst(_, v) => clAtomicConst(env)(v)
    case x:imir.MetaConst => clMetaConst(env)(x)
    case imir.StructConst(f, isPacked) => llvm.StructConst(f.map(clTC(env)), isPacked)
    case imir.VectorConst(f) => llvm.VectorConst(f.map(clTC(env)))
    case imir.ArrayConst(e) => llvm.ArrayConst(e.map(clTC(env)))
    case imir.StrConst(str) => llvm.StrConst(str)
    case imir.LabelQuoteStr(str) => llvm.LabelQuoteStr(str)
    case imir.BlockAddress(a1, a2) => llvm.BlockAddress(clGlobalVar(env)(a1), clLocalVar(env)(a2))
    case imir.ConstArithmaticExpr(bop, c, tv, op1) => llvm.ConstArithmaticExpr(bop, c, clTC(env)(tv), clTC(env)(op1))
    case imir.ConstBitwiseExpr(bop, tv, op1) => llvm.ConstBitwiseExpr(bop, clTC(env)(tv), clTC(env)(op1))
    case imir.ConstCast(conv, tv, t) => llvm.ConstCast(conv, clTC(env)(tv), clType(env)(t))
    case imir.ConstGetElemPtr(isInbounds, base, l) => llvm.ConstGetElemPtr(isInbounds, clTC(env)(base), l.map(clTC(env)))
    case imir.ConstSelect(cond, v1, v2) => llvm.ConstSelect(clTC(env)(cond), clTC(env)(v1), clTC(env)(v2))
    case imir.ConstICmpExpr(cmp, tv, v) => llvm.ConstICmpExpr(cmp, clTC(env)(tv), clTC(env)(v))
    case imir.ConstFCmpExpr(cmp, tv, v) => llvm.ConstFCmpExpr(cmp, clTC(env)(tv), clTC(env)(v))
    case imir.ConstExtract (tv, idx) => llvm.ConstExtract(clTC(env)(tv), idx)
    case imir.ConstInsertElement(vect, tv, index) => llvm.ConstInsertElement(clTC(env)(vect), clTC(env)(tv), clTC(env)(index))
    case imir.ConstShuffleVector(vect1, vect2, mask) => llvm.ConstShuffleVector(clTC(env)(vect1), clTC(env)(vect2), clTC(env)(mask))
    case imir.ConstExtractValue(tv, indices) =>  llvm.ConstExtractValue(clTC(env)(tv), indices)
    case imir.ConstInsertValue(vect, tv, indices) => llvm.ConstInsertValue(clTC(env)(vect), clTC(env)(tv), indices)
    case imir.ConstExtractElement(tv, index) => llvm.ConstExtractElement(clTC(env)(tv), clTC(env)(index))
  }

  def clRHS(env:imir.ImEnv)(x:imir.RHS) = x match {
    case e:imir.Expr => clExpr(env)(e)
    case e:imir.MemOp => clMemOp(env)(e)
    case imir.Phi(ty, ins) => llvm.Phi(clType(env)(ty), ins.map(t => (clValue(env)(t._1), clValue(env)(t._2))))
    case imir.Call(cconv, retAttrs, retType, fn, actualParams, funAttrs) =>
      llvm.Call(cconv, retAttrs, clType(env)(retType), clVarOrID(env)(fn), clActualParamList(env)(actualParams), funAttrs)
    case imir.ExtractElement(tv, index) => llvm.ExtractElement(clTV(env)(tv), clTV(env)(index))
    case imir.InsertElement(vect, tv, index) => llvm.InsertElement(clTV(env)(vect), clTV(env)(tv), clTV(env)(index))
    case imir.ShuffleVector(vect1, vect2, mask) =>
      llvm.ShuffleVector(clTV(env)(vect1), clTV(env)(vect2), clTV(env)(mask))
    case imir.ExtractValue(tv, indices) =>
    	 llvm.ExtractValue(clTV(env)(tv), indices)
    case imir.InsertValue(vect, tv, indices) =>
      llvm.InsertValue(clTV(env)(vect), clTV(env)(tv), indices)
    case imir.VaArg(tv, v) => llvm.VaArg(clTV(env)(tv), clType(env)(v))
  }

  def clExpr(env:imir.ImEnv)(x:imir.Expr) = x match {
    case imir.ConstExpr(const) =>
      llvm.CastExpr(Bitcast, clTV(env)(const), clType(env)(env.typeOf(const)))
    case imir.ArithmaticExpr(binOp, carry, t, v1, v2) =>
      llvm.ArithmaticExpr(binOp, carry, clType(env)(t), clValue(env)(v1), clValue(env)(v2))
    case imir.ICmpExpr(cmp, t, v1, v2) => llvm.ICmpExpr(cmp, clType(env)(t), clValue(env)(v1), clValue(env)(v2))
    case imir.FCmpExpr(cmp, t, v1, v2) => llvm.FCmpExpr(cmp, clType(env)(t), clValue(env)(v1), clValue(env)(v2))
    case imir.BitwiseExpr(binOp, t, v1, v2) => llvm.BitwiseExpr(binOp, clType(env)(t), clValue(env)(v1), clValue(env)(v2))
    case imir.GetElemPtr(tv, indices) => llvm.GetElemPtr(clTV(env)(tv), indices.map(clTC(env)))
    case imir.GetResult(tv, index) => llvm.GetResult(clTV(env)(tv), index)
    case imir.CastExpr(op, tv, t) => llvm.CastExpr(op, clTV(env)(tv), clType(env)(t))
    case imir.Select (c, t, v) => llvm.Select(clTV(env)(c), clTV(env)(t), clTV(env)(v))
    case imir.Extract (tv, idx) => llvm.Extract(clTV(env)(tv), idx)
   }

  def clLabel(env:imir.ImEnv)(x:imir.Label) = x match { case imir.Label(s) => llvm.Label(s) }

  def clMemOp(env:imir.ImEnv)(x:imir.MemOp) = x match {
    case imir.Alloca(ty, size, align) => llvm.Alloca(clType(env)(ty), size.map(clTV(env)), align)
    case imir.Malloc(ty, size, align) => llvm.Malloc(clType(env)(ty), size.map(clTV(env)), align)
    case imir.Free(tv) => llvm.Free(clTV(env)(tv))
    case imir.Load(volatile, ptr, align) => llvm.Load(volatile, clTV(env)(ptr), align)
    case imir.Store(volatile, v, ptr, align) => llvm.Store(volatile, clTV(env)(v), clTV(env)(ptr), align)
  }
  
  def clTV(env:imir.ImEnv)(x:imir.Value):llvm.TV = {
    val t = env.typeOf(x)
    new llvm.TV(clType(env)(t), clValue(env)(x))
  }

  def clTC(env:imir.ImEnv)(x:imir.Const):llvm.TC = {
    val t =  env.typeOf(x)
    new llvm.TC(clType(env)(t), clConst(env)(x))
  }


  def clInstruction(env:imir.ImEnv)(x:imir.AbsInst) =  {
    def f(y:imir.Instruction) =
      llvm.Instruction(y.lhs.map(clVarOrID(env)), clRHS(env)(y.rhs), List())
    x match {
      case y:imir.Instruction => f(y)
      case imir.Comment(s) => llvm.Comment(s)
    }
  }

  def clControlInst(env:imir.ImEnv)(x:imir.ControlInst):llvm.ControlInst = x match {
    case imir.Unreachable() => llvm.Unreachable()
    case imir.Ret (v) => llvm.Ret(v.map(clTV(env)))
    case imir.Br(l) => llvm.Br(clTV(env)(l))
    case imir.CBr(cond, tru, fal) => llvm.CBr(clTV(env)(cond), clTV(env)(tru), clTV(env)(fal))
    case imir.IndirectBr(cond, branchs) => llvm.IndirectBr(clTV(env)(cond), branchs.map(clTV(env)))
    case imir.Switch(v, default, jumptable) =>
      llvm.Switch(clTV(env)(v), clTV(env)(default), jumptable.map(t => (clTV(env)(t._1), clTV(env)(t._2))))
    case imir.Invoke(lhs, cconv, retAttrs, fn, actualParams, funAttrs, label, exception) =>  {
      val nlhs = lhs.map(clVarOrID(env))
      llvm.Invoke(nlhs, cconv, retAttrs, clTV(env)(fn), clActualParamList(env)(actualParams), funAttrs, clTV(env)(label),
        clTV(env)(exception))
    }
    case imir.Unwind() => llvm.Unwind()
  }

  def clControlInstDbg(env:imir.ImEnv)(x:imir.ControlInstDbg):llvm.ControlInstDbg = x match {
    case imir.ControlInstDbg(inst, dbg) =>
      llvm.ControlInstDbg(clControlInst(env)(inst), dbg.map(clDbg(env)))
  }

  def clAbsNodeToLabel(env:imir.ImEnv)(n:AbsNode):llvm.Label = n match {
    case imir.FirstNode(l,e) => llvm.Label(l)
    case _ => throw new Exception()
  }

  def clAbsNodeToAbsInst(env:imir.ImEnv)(n:AbsNode):llvm.AbsInst = n match {
    case imir.MiddleNode(n) => clInstruction(env)(n)
    case _ => throw new Exception()
  }

  def clAbsNodeToCtrl(env:imir.ImEnv)(n:AbsNode):llvm.ControlInst = n match {
    case imir.LastNode(targets, n) => clControlInst(env)(n)
    case _ => throw new Exception()
  }

  def clBlock(env:imir.ImEnv)(x:Block[AbsNode,C,C]) = x match {
    case BClosed(hd, tail) => {
      def clMiddle(middle:Block[AbsNode,O,O]):List[llvm.AbsInst] = middle match {
        case BCat(m1, m2) => clMiddle(m1):::clMiddle(m2)
        case BMiddle(n) => List(clAbsNodeToAbsInst(env)(n))
      }
      def clHead (hd:Block[AbsNode,C,O]):(Option[llvm.Label], List[llvm.AbsInst]) = hd match {
        case BHead(first, middle) => {
          val (opt, fl) = clHead(first)
          (opt, fl:::clMiddle(middle))
        }
        case BFirst(n) => (Some(clAbsNodeToLabel(env)(n)), List())
      }
      def clTail(tail:Block[AbsNode,O,C]):(List[llvm.AbsInst], llvm.ControlInst) = tail match {
        case BTail(middle, last) => {
          val ml = clMiddle(middle)
          val (ll, ctrl) = clTail(last)
          (ml:::ll, ctrl)
        }
        case BLast(n) => (List(), clAbsNodeToCtrl(env)(n))
      }
      val (optLbl, inst1) = clHead(hd)
      val (inst2, ctrlInst) = clTail(tail)
      llvm.Block(optLbl, 0, inst1:::inst2, llvm.ControlInstDbg(ctrlInst, List()))
    }
    case _ => throw new Exception()
  }

  def clFormalParam(env:imir.ImEnv)(x:imir.FormalParam) = x match {
    case imir.FormalParam(ty, attr1, align, id, attr2) =>
      llvm.FormalParam(clType(env)(ty), attr1, align, id.map(clLocalVarOrID(env)), attr2)
  }

  def clFormalParamList(env:imir.ImEnv)(x:imir.FormalParamList) = x match {
    case imir.FormalParamList(list, hasDots, funAttrs) =>
      llvm.FormalParamList(list.map(clFormalParam(env)), hasDots, funAttrs)
  }

  def clActualParam(env:imir.ImEnv)(x:imir.ActualParam) = x match {
    case imir.ActualParam(attrs1, align, v, attrs2) => {
      val tv = clTV(env)(v)
      llvm.ActualParam(tv.ty, attrs1, align, tv.v, attrs2)
    }
  }

  def clActualParamList(env:imir.ImEnv)(x:imir.ActualParamList) = x match {
    case imir.ActualParamList(list) => llvm.ActualParamList(list.map(clActualParam(env)))
  }

  def clTopLevel(env:imir.ImEnv)(x:imir.TopLevel) = x match {
    case imir.Target(k, v) => llvm.Target(k, v)
    case y:imir.Declare => clDecare(env)(y)
    case imir.TypeDef(name, ty) => {
      val nty = clType(env)(ty)
      llvm.TypeDef(name, nty)
    }
    case imir.UnamedType(id, ty) => {
      val nty = clType(env)(ty)
      llvm.UnamedType(id, nty)
    }
    case imir.DepLibs(l) => llvm.DepLibs(l)
    case imir.ModuleAsm(str) => llvm.ModuleAsm(str)
    case x:imir.Alias => clAlias(env)(x)
    case x:imir.Global => clGlobal(env)(x)
    case imir.StandardaloneMD(lhs, ty) => llvm.StandardaloneMD(lhs, clTV(env)(ty))
    case imir.NamedMD(lhs, rhs) => llvm.NamedMD(clMDVar(env)(lhs), rhs.map(clMDNode(env)))
    case x:imir.DbgInit => clDbgInit(env)(x)
    case x:imir.FunctionDef => clFunctionDef(env)(x)
  }

  def clDecare(env:imir.ImEnv)(x:imir.Declare) = {
    def f(y:imir.Declare) = llvm.Declare(clFunctionHeader(env)(y.header))
    f(x)
  }

  def clAliasee(env:imir.ImEnv)(x:imir.Aliasee) = x match {
    case imir.AliaseeTV(tv) => llvm.AliaseeTV(clTV(env)(tv))
    case imir.AliaseeBitCast(tv, t) => llvm.AliaseeBitCast(clTV(env)(tv), clType(env)(t))
    case imir.AliaseeGetElemPtr(get) => llvm.AliaseeGetElemPtr(clConstGetElemPtr(env)(get))
  }

  def clAlias(env:imir.ImEnv)(x:imir.Alias) = {
    def f(y:imir.Alias)=
      llvm.Alias(y.lhs.map(clGlobalVarOrID(env)), y.visibility, y.linkage, clAliasee(env)(y.aliasee))
    f(x)
  }

  def clGlobal(env:imir.ImEnv)(x:imir.Global) = {
    def f(x:imir.Global) =
      llvm.Global(x.lhs.map(clGlobalVarOrID(env)), x.linkage, x.visiblilty,
        x.threadLocation,  x.addrSpace, x.globalType, clType(env)(x.ty),
        x.const.map(clConst(env)), x.section, x.align)
    f(x)
  }

  def clStandardaloneMD(env:imir.ImEnv)(x:imir.StandardaloneMD) = x match {
    case imir.StandardaloneMD(lhs, ty) => llvm.StandardaloneMD(lhs, clTV(env)(ty))
  }

  def clNamedMD(env:imir.ImEnv)(x:imir.NamedMD) = x match {
    case imir.NamedMD(lhs, rhs) => llvm.NamedMD(clMDVar(env)(lhs), rhs.map(clMDNode(env)))
  }

  def clDbgInit(env:imir.ImEnv)(x:imir.DbgInit) = x match {
    case imir.DbgInit(lhs, start) => llvm.DbgInit(lhs, start)
  }

  def clFunctionHeader(env:imir.ImEnv)(x:imir.FunctionHeader) = {
    def f(y:imir.FunctionHeader) =
      llvm.FunctionHeader(y.linkage, y.visibility, y.cconv, y.retAttrs,
        clGlobalVarOrID(env)(y.name), clFunType(env)(y.funType), y.section, y.align, y.gc)
    f(x)
  }

  def clFunctionDef(env:imir.ImEnv)(x:imir.FunctionDef) = x match {
    case imir.FunctionDef (fenv, funProto, g) =>
      val (start, g1) = g.postorderDfs
      val l = g1.map(clBlock(fenv))
      llvm.FunctionDef(clFunctionHeader(fenv)(funProto), l)
  }

  def clDbg(env:imir.ImEnv)(x:imir.Dbg) = x match {
    case imir.Dbg(s, meta) => llvm.Dbg(s, clMetaConst(env)(meta))
  }
  def clModule() = m match {
    case imir.Module(list, env) => llvm.Module(list.map(clTopLevel(env)))
  }
}