package org.scalair.ir

import org.scalair._
import org.scalair.hoopl._
import imir.{ImEnv, DataLayout}
import pass.DominatorTree
import org.scalair.util.Logging

/**
 * User: wangn
 * Date: Mar 8, 2011
 */

final case class Llvm2ImIr(m:llvm.Module, blockMap:Map[llvm.GlobalVarOrID, Map[Int, llvm.Block]])
  extends GraphBuilder[imir.AbsNode] with Logging
{
  var env = ImEnv.functionScope(DataLayout())

  def processBlocks(name:llvm.GlobalVarOrID) = {
    env = blockMap.get(name).foldLeft(env) { (env, bm) =>
      bm.foldLeft(env) { (env, x) =>
        x._2.label.foldLeft(env) { (env,l) =>
          env.addLabelDef(l.s, x._1)
        }
      }
    }
  }

  def addDef(x:imir.VarOrID, v:imir.Value) {
    env = x match {
      case imir.GlobalID(id) => env.addGlobalDef(id, v)
      case imir.GlobalVar(s) => env.addGlobalDef(s, v)
      case imir.LocalID(id) => env.addLocalDef(id, v)
      case imir.LocalVar(s) => env.addLocalDef(s, v)
      case imir.LocalStr(s) => env.addLocalDef(s.toString, v)
    }
  }

  def clFunType(x:llvm.FunType):imir.FunType = x match {
    case llvm.FunType(ret, fp, fa) => imir.FunType(clType(ret), clFormalParamList(fp), fa)
  }

  def clType(x:llvm.Type):imir.Type = x match {
    case llvm.I(i) => imir.I(i)
    case llvm.F(i) => imir.F(i)
    case llvm.V64() => imir.V64()
    case llvm.V128() => imir.V128()
    case llvm.VoidType() => imir.VoidType()
    case llvm.FloatType() => imir.FloatType()
    case llvm.DoubleType() => imir.DoubleType()
    case llvm.FP128Type() => imir.FP128Type()
    case llvm.X86FP80Type() => imir.X86FP80Type()
    case llvm.PPCFP128Type() => imir.PPCFP128Type()
    case llvm.X86MMXType() => imir.X86MMXType()
    case llvm.LabelType() => imir.LabelType()
    case llvm.MetadataType() => imir.MetadataType()
    case llvm.OpaqueType() => imir.OpaqueType()
    case llvm.TypeName(n) => imir.TypeName(n)
    case llvm.TypeNo(no) =>  imir.TypeNo(no)
    case llvm.TypeUpRef(no) => imir.TypeUpRef(no)
    case llvm.ArrayType(n, b) => imir.ArrayType(n, clType(b))
    case llvm.VectorType(n, b) => imir.VectorType(n, clType(b))
    case llvm.StructType(e, p) => imir.StructType(e.map(clType), p)
    case llvm.PointerType(a, b) => imir.PointerType(clType(a), b)
    case llvm.FunType(ret, fp, fa) => imir.FunType(clType(ret), clFormalParamList(fp), fa)
  }

  def clValue(t:imir.Type, x:llvm.Value):imir.Value = x match {
    case x:llvm.VarOrID => val x0 = clVarOrID(x); env = env.add(x0, t); x0
    case x:llvm.Const => clConst(t)(x)
    case x:llvm.InlineAsm => val x0 = clInlineAsm(x); env = env.add(x0, t); x0
  }

  def clVarOrID(x:llvm.VarOrID) = x match {
    case x:llvm.GlobalVarOrID => clGlobalVarOrID(x)
    case x:llvm.LocalVarOrID => clLocalVarOrID(x)
  }

  def clGlobalVar(x:llvm.GlobalVar) = x match {
    case llvm.GlobalVar(s) => imir.GlobalVar(s)
  }

  def clGlobalVarOrID(x:llvm.GlobalVarOrID) = x match {
    case llvm.GlobalID(n) => imir.GlobalID(Integer.parseInt(n))
    case llvm.GlobalVar(n) => imir.GlobalVar(n)
  }

  def clLocalVar(x:llvm.LocalVar) = x match {
    case llvm.LocalVar(n) => imir.LocalVar(n)
  }

  def clLocalVarOrID(x:llvm.LocalVarOrID) = x match {
    case llvm.LocalID(n) => imir.LocalID(Integer.parseInt(n))
    case llvm.LocalVar(n) => imir.LocalVar(n)
    case llvm.LocalStr(n) => imir.LocalStr(n)
  } 

  def clMetaConst(x:llvm.MetaConst) = x match {
    case llvm.MDConst(c) => imir.MDConst(clConst(imir.VoidType())(c))
    case llvm.MDNode(n) => imir.MDNode(n)
    case llvm.MDString(n) => imir.MDString(n)
    case llvm.MDVar(n) => imir.MDVar(n)
    case llvm.MDRef(n) => imir.MDRef(clLocalVarOrID(n))
  }

  def clMDVar(x:llvm.MDVar) = x match {
    case llvm.MDVar(n) => imir.MDVar(n)
  }

  def clMDNode(x:llvm.MDNode) = x match {
    case llvm.MDNode(n) => imir.MDNode(n)
  }

  def clInlineAsm(x:llvm.InlineAsm) = x match { 
    case llvm.InlineAsm(h, a, s1, s2) => imir.InlineAsm(h, a, s1, s2)
  }

  def clAtomicConst(x:llvm.AtomicConst):imir.ConstLiteral = x match {
    case llvm.IntConst(str) => imir.IntConst(str)
    case llvm.FloatConst(str) => imir.FloatConst(str)
    case llvm.NullValue() => imir.NullValue()
    case llvm.Undef() => imir.Undef()
    case llvm.BTrue() => imir.BTrue()
    case llvm.BFalse() => imir.BFalse()
    case llvm.Zero() => imir.Zero()
    case llvm.GlobalAddr(g) => throw new Exception("error")
  }

  def clConstGetElemPtr(x:llvm.ConstGetElemPtr) = x match {
    case llvm.ConstGetElemPtr(isInbounds, base, indices) =>
      imir.ConstGetElemPtr(isInbounds, clTC(base), indices.map(clTC))
  }

  def clConst(t:imir.Type)(x:llvm.Const):imir.Const = x match {
    case llvm.GlobalAddr(g) => imir.GlobalAddr(clGlobalVarOrID(g))
    case x:llvm.AtomicConst => imir.PrimitiveConst(t, clAtomicConst(x))
    case x:llvm.MetaConst => clMetaConst(x)
    case llvm.StructConst(f, isPacked) => imir.StructConst(f.map(clTC), isPacked)
    case llvm.VectorConst(f) => imir.VectorConst(f.map(clTC))
    case llvm.ArrayConst(e) => imir.ArrayConst(e.map(clTC))
    case llvm.StrConst(str) => val x0 = imir.StrConst(str); env = env.add(x0, t); x0
    case llvm.LabelQuoteStr(str) => val x0 = imir.LabelQuoteStr(str); env = env.add(x0, t); x0
    case llvm.BlockAddress(a1, a2) => val x0 = imir.BlockAddress(clGlobalVar(a1), clLocalVar(a2)); env = env.add(x0, t); x0
    case llvm.ConstArithmaticExpr(bop, c, tv, op1) => imir.ConstArithmaticExpr(bop, c, clTC(tv), clTC(op1))
    case llvm.ConstBitwiseExpr(bop, tv, op1) => imir.ConstBitwiseExpr(bop, clTC(tv), clTC(op1))
    case llvm.ConstCast(conv, tv, t) => imir.ConstCast(conv, clTC(tv), clType(t))
    case llvm.ConstGetElemPtr(isInbounds, base, indices) => imir.ConstGetElemPtr(isInbounds, clTC(base), indices.map(clTC))
    case llvm.ConstSelect(cond, v1, v2) => imir.ConstSelect(clTC(cond), clTC(v1), clTC(v2))
    case llvm.ConstICmpExpr(cmp, tv, v) => imir.ConstICmpExpr(cmp, clTC(tv), clTC(v))
    case llvm.ConstFCmpExpr(cmp, tv, v) => imir.ConstFCmpExpr(cmp, clTC(tv), clTC(v))
    case llvm.ConstExtract (tv, idx) => imir.ConstExtract(clTC(tv), idx)
    case llvm.ConstInsertElement(vect, tv, index) => imir.ConstInsertElement(clTC(vect), clTC(tv), clTC(index))
    case llvm.ConstShuffleVector(vect1, vect2, mask) => imir.ConstShuffleVector(clTC(vect1), clTC(vect2), clTC(mask))
    case llvm.ConstExtractValue(tv, indices) =>  imir.ConstExtractValue(clTC(tv), indices)
    case llvm.ConstInsertValue(vect, tv, indices) => imir.ConstInsertValue(clTC(vect), clTC(tv), indices)
    case llvm.ConstExtractElement(tv, index) => imir.ConstExtractElement(clTC(tv), clTC(index))
  }

  def clRHS(x:llvm.RHS):(imir.RHS, imir.Type) = x match {
    case e:llvm.Expr => val e0 = clExpr(e); (e0, env.typeOf(e0))
    case e:llvm.MemOp => clMemOp(e)
    case llvm.Phi(ty, ins) => {
      val ty0 = clType(ty)
      (imir.Phi(ty0, ins.map(t => (clValue(clType(ty), t._1), clValue(clType(ty), t._2)))), ty0)
    }
    case llvm.Call(cconv, retAttrs, retType, fn, actualParams, funAttrs) =>  {
      val retType0 = clType(retType)
      val fn0 = clVarOrID(fn)
      val e0 = imir.Call(cconv, retAttrs, retType0, fn0, clActualParamList(actualParams), funAttrs)
      (e0, retType0)
    }
    case llvm.ExtractElement(tv, index) => {
      val e0 = imir.ExtractElement(clTV(tv), clTV(index))
      (e0, env.typeOf(e0))
    }
    case llvm.InsertElement(vect, tv, index) => {
      val e0 = imir.InsertElement(clTV(vect), clTV(tv), clTV(index))
      (e0, env.typeOf(e0))
    }
    case llvm.ShuffleVector(vect1, vect2, mask) => {
      val e0 = imir.ShuffleVector(clTV(vect1), clTV(vect2), clTV(mask))
      (e0, env.typeOf(e0))
    }
    case llvm.ExtractValue(tv, indices) => {
      val e0 = imir.ExtractValue(clTV(tv), indices)
      (e0, env.typeOf(e0))
    }
    case llvm.InsertValue(vect, tv, indices) => {
      val e0 = imir.InsertValue(clTV(vect), clTV(tv), indices)
      (e0, env.typeOf(e0))
    }
    case llvm.VaArg(tv, ty) => {
      val ty0 = clType(ty)
      (imir.VaArg(clTV(tv), ty0), ty0)
    }
  }

  def clExpr(x:llvm.Expr) = x match {
    case llvm.ArithmaticExpr(binOp, carry, t, v1, v2) =>
      imir.ArithmaticExpr(binOp, carry, clType(t), clValue(clType(t), v1), clValue(clType(t), v2))
    case llvm.ICmpExpr(cmp, t, v1, v2) => imir.ICmpExpr(cmp, clType(t), clValue(clType(t), v1), clValue(clType(t),v2))
    case llvm.FCmpExpr(cmp, t, v1, v2) => imir.FCmpExpr(cmp, clType(t), clValue(clType(t), v1), clValue(clType(t), v2))
    case llvm.BitwiseExpr(binOp, t, v1, v2) => imir.BitwiseExpr(binOp, clType(t), clValue(clType(t), v1), clValue(clType(t),v2))
    case llvm.GetElemPtr(tv, indices) => imir.GetElemPtr(clTV(tv), indices.map(clTC))
    case llvm.GetResult(tv, index) => imir.GetResult(clTV(tv), index)
    case llvm.CastExpr(op, tv, t) => imir.CastExpr(op, clTV(tv), clType(t))
    case llvm.Select (c, t, v) => imir.Select(clTV(c), clTV(t), clTV(v))
    case llvm.Extract (tv, idx) => imir.Extract(clTV(tv), idx)
   }

  def clLabel(x:llvm.Label) = x match { case llvm.Label(s) => imir.Label(s) }

  def clMemOp(x:llvm.MemOp):(imir.MemOp, imir.Type) = x match {
    case llvm.Alloca(ty, size, align) => {
      val ty0 = clType(ty)
      (imir.Alloca(ty0, size.map(clTV), align), imir.PointerType(ty0))
    }
    case llvm.Malloc(ty, size, align) => {
      val ty0 = clType(ty)
      (imir.Malloc(ty0, size.map(clTV), align), imir.PointerType(ty0))
    }
    case llvm.Free(tv) => (imir.Free(clTV(tv)), imir.VoidType())
    case llvm.Load(volatile, ptr, align) => {
      val ptr0 = clTV(ptr)
      (imir.Load(volatile, ptr0, align), env.typeOf(ptr0))
    }
    case llvm.Store(volatile, v, ptr, align) =>
      (imir.Store(volatile, clTV(v), clTV(ptr), align), imir.VoidType())
  }
  
  def clTV(x:llvm.TV) = x match {
    case x:llvm.TC => clTC(x)
    case llvm.TV(ty, v) => clValue(clType(ty), v)
  }

  def getTargets(x:llvm.TV):List[Int] = x match {
    case llvm.TV(llvm.LabelType(), t) => t match {
      case llvm.LocalVar(x) => env.getLabelDef(x) match {
        case Some(id) => id::Nil
        case None => List()
      }
      case llvm.LocalID(x) => env.getLabelDef(x) match {
        case Some(id) => id::Nil
        case None => List()
      }
    }
    case _ => throw new Exception()
  }

  def clTC(x:llvm.TC):imir.Const = x match {
    case llvm.NullTV() => imir.PrimitiveConst(imir.PointerType(imir.I(8)), imir.NullValue())
    case llvm.TC(ty, c) => clConst(clType(ty))(c)
  }

  def clInstruction(x:llvm.AbsInst) = x match {
    case llvm.Instruction(lhs, rhs, dbg) => {
      val nlhs = lhs.map(clVarOrID)
      val (rhs0, ty0) = clRHS(rhs)
      val nval = imir.Instruction(nlhs, rhs0, dbg.map(clDbg))
      env = env.add(nlhs, ty0)
      nlhs match {
        case Some(lhs) => addDef (lhs, nval);
        case None => ()
      }
      nval
    }
    case llvm.Comment(s) => imir.Comment(s)
  }

  def clControlInst(x:llvm.ControlInst):imir.ControlInst = x match {
    case llvm.Unreachable() => imir.Unreachable()
    case llvm.Ret (v) => imir.Ret(v.map(clTV))
    case llvm.Br(l) => imir.Br(clTV(l))
    case llvm.CBr(cond, tru, fal) => imir.CBr(clTV(cond), clTV(tru), clTV(fal))
    case llvm.IndirectBr(cond, branchs) => imir.IndirectBr(clTV(cond), branchs.map(clTV))
    case llvm.Switch(v, default, jumptable) =>
      imir.Switch(clTV(v), clTV(default), jumptable.map(t => (clTV(t._1), clTV(t._2))))
    case llvm.Invoke(lhs, cconv, retAttrs, fn, actualParams, funAttrs, label, exception) =>  {
      val nlhs = lhs.map(clVarOrID)
      val nval = imir.Invoke(nlhs, cconv, retAttrs, clTV(fn), clActualParamList(actualParams),
        funAttrs, clTV(label), clTV(exception))
      nlhs match {
        case Some(lhs) => addDef (lhs, nval)
        case None => ()
      }
      nval
    }
    case llvm.Unwind() => imir.Unwind()
  }

  def clControlInstDbg(x:llvm.ControlInstDbg):imir.ControlInstDbg = x match {
    case llvm.ControlInstDbg(inst, dbg) => imir.ControlInstDbg(clControlInst(inst), dbg.map(clDbg))
  }

  def mkLastNode(inst:llvm.ControlInst):imir.LastNode = inst match {
    case llvm.Unreachable() => imir.LastNode(List(), imir.Unreachable())
    case llvm.Ret (v) => imir.LastNode(List(), imir.Ret(v.map(clTV)))
    case llvm.Br(l) => imir.LastNode(getTargets(l), imir.Br(clTV(l)))
    case llvm.CBr(cond, tru, fal) => {
      val succ = getTargets(tru):::getTargets(fal)
      imir.LastNode(succ, imir.CBr(clTV(cond), clTV(tru), clTV(fal)))
    }
    case llvm.IndirectBr(cond, branchs) => {
      val succ = branchs.flatMap(getTargets(_))
      imir.LastNode(succ, imir.IndirectBr(clTV(cond), branchs.map(clTV)))
    }
    case llvm.Switch(v, default, jumptable) => {
      val succ = List()
      imir.LastNode(succ, imir.Switch(clTV(v), clTV(default), jumptable.map(t => (clTV(t._1), clTV(t._2)))))
    }
    case llvm.Invoke(lhs, cconv, retAttrs, fn, actualParams, funAttrs, label, exception) => {
      val nlhs = lhs.map(clVarOrID)
      val nval = imir.Invoke(nlhs, cconv, retAttrs, clTV(fn), clActualParamList(actualParams),
        funAttrs, clTV(label), clTV(exception))
      nlhs match {
        case Some(lhs) => addDef (lhs, nval)
        case None => ()
      }
      imir.LastNode(List(), nval)
    }
    case llvm.Unwind() => imir.LastNode(List(), imir.Unwind())
  }


  def clEntryNode(block:llvm.Block) = {
    block.label match {
      case Some(l) => {
        val lastNode = mkLastNode(llvm.Br(new llvm.TV(llvm.LabelType(), llvm.LocalVar(l.s))))
        val lastG = mkLast(lastNode)
        lastG
      }
      case _ => throw new Exception("")
    }
  }

  // todo: need to fix this if entry block is a jump target
  def clEntryBlock(x:llvm.Block):(Graph[imir.AbsNode,O,C]) = x match {
    case llvm.Block(label, index, middle, end) => {
      val label1 = label.map(clLabel)
      val middle1 = middle.map(clInstruction)
      val end1 = clControlInstDbg(end)
      //val block = imir.Block(label1, index, middle1, end1);
      val startNode = label1 match {
        case Some(x) => env.getLabelDef(x.s) match {
          case Some(n) => imir.StartNode(x.s, index);
          case None => throw new Exception(x.s + " is no defined")
        }
        case None => imir.StartNode("<undefined>", index)
      }
      val middleNodes = middle1.map(imir.MiddleNode)
      val lastNode = mkLastNode(end.inst)

      val middleN = mkMiddles(startNode::middleNodes)
      val lastN = mkLast(lastNode)
      val g = middleN.splice[C](lastN)
      //Console.println(g.toDot("entryblock"))
      g
    }
  }

  def clBlock(x:llvm.Block):Graph[imir.AbsNode,C,C] = x match {
    case llvm.Block(label, index, middle, end) => {
      val label1 = label.map(clLabel)
      val middle1 = middle.map(clInstruction)
      val end1 = clControlInstDbg(end)
      //val block = imir.Block(label1, index, middle1, end1);
      val lnode = label1 match {
        case Some(x) => env.getLabelDef(x.s) match {
          case Some(n) => imir.FirstNode(x.s, index);
          case None => throw new Exception(x.s + " is no defined")
        }
        case None => imir.FirstNode("<undefined>", index)
      }
      val middleNodes = middle1.map(imir.MiddleNode)
      val lastNode = mkLastNode(end.inst)

      val firstN = mkFirst(lnode)
      val middleN = mkMiddles(middleNodes)
      val lastN = mkLast(lastNode)
      val g = firstN.splice[O](middleN).splice[C](lastN)
      //Console.println(g.toDot("1block"))
      g
    }
  }

  def clFormalParam(x:llvm.FormalParam) = x match {
    case llvm.FormalParam(ty, attr1, align, id, attr2) => {
      val ty0 = clType(ty)
      val id0 = id.map(clLocalVarOrID)
      env = env.add(id0, ty0)
      imir.FormalParam(ty0, attr1, align, id0, attr2)
    }
  }

  def clFormalParamList(x:llvm.FormalParamList) = x match {
    case llvm.FormalParamList(list, hasDots, funAttrs) =>
      imir.FormalParamList(list.map(clFormalParam), hasDots, funAttrs)
  }

  def clActualParam(x:llvm.ActualParam) = x match {
    case llvm.ActualParam(ty, attrs1, align, v, attrs2) =>
      imir.ActualParam(attrs1, align, clValue(clType(ty), v), attrs2)
  }

  def clActualParamList(x:llvm.ActualParamList) = x match {
    case llvm.ActualParamList(list) => imir.ActualParamList(list.map(clActualParam))
  }

  def enclose[I,O](i:I, f:I=>O):O = {
    val prev = env
    val o = f(i)
    env = prev
    o
  }

  def clTopLevel(x:llvm.TopLevel) = x match {
    case llvm.Target(k, v) => imir.Target(k, v)
    case llvm.Declare(header) => {
      val header0 = enclose(header, clFunctionHeader)
      env = env.add(header0.name, header0.funType)
      imir.Declare(header0)
    }
    case llvm.TypeDef(name, ty) =>{
      val nty = clType(ty)
      env = env.addTypeDef(name, nty)
      imir.TypeDef(name, nty)
    }
    case llvm.UnamedType(id, ty) => {
      val nty = clType(ty)
      env = env.addTypeDef(id, nty)
      imir.UnamedType(id, nty)
    }
    case llvm.DepLibs(l) => imir.DepLibs(l)
    case llvm.ModuleAsm(str) => imir.ModuleAsm(str)
    case x:llvm.Alias => clAlias(x)
    case x:llvm.Global => clGlobal(x)
    case llvm.StandardaloneMD(lhs, ty) => imir.StandardaloneMD(lhs, clTV(ty))
    case llvm.NamedMD(lhs, rhs) => imir.NamedMD(clMDVar(lhs), rhs.map(clMDNode))
    case x:llvm.DbgInit => clDbgInit(x)
    case x:llvm.FunctionDef => {
      val f = enclose(x, clFunctionDef)
      env = env.add(f.funProto.name, f.funProto.funType)
      f
    }
  }

  def clAliasee(x:llvm.Aliasee) = x match {
    case llvm.AliaseeTV(tv) => imir.AliaseeTV(clTV(tv))
    case llvm.AliaseeBitCast(tv, t) => imir.AliaseeBitCast(clTV(tv), clType(t))
    case llvm.AliaseeGetElemPtr(get) => imir.AliaseeGetElemPtr(clConstGetElemPtr(get))
  }


  def clAlias(x:llvm.Alias) = x match {
    case llvm.Alias(lhs, visibility, linkage, aliasee) =>
      imir.Alias(lhs.map(clGlobalVarOrID), visibility, linkage, clAliasee(aliasee))
  }

  def clGlobal(x:llvm.Global) = x match {
    case llvm.Global(lhs, linkage, visiblilty, threadLocation, addrSpace, globalType, ty, const, section, align) => {
      val lhs0 = lhs.map(clGlobalVarOrID)
      val ty0 = clType(ty)
      env = env.add(lhs0, ty0)
      imir.Global(lhs0, linkage, visiblilty,
        threadLocation,  addrSpace, globalType, ty0, const.map(clConst(ty0)), section, align)
    }
  }

  def clStandardaloneMD(x:llvm.StandardaloneMD) = x match {
    case llvm.StandardaloneMD(lhs, ty) => imir.StandardaloneMD(lhs, clTV(ty))
  }
    
  def clNamedMD(x:llvm.NamedMD) = x match {
  	case llvm.NamedMD(lhs, rhs) => imir.NamedMD(clMDVar(lhs), rhs.map(clMDNode))
  }

  def clDbgInit(x:llvm.DbgInit) = x match {
    case llvm.DbgInit(lhs, start) => imir.DbgInit(lhs, start)
  }

  def clFunctionHeader(x:llvm.FunctionHeader) = x match {
    case llvm.FunctionHeader (linkage, visibility, cconv, retAttrs, name, funType, section, align, gc) => {
      val name0 = clGlobalVarOrID(name)
      val funType0 = clFunType(funType)
      processBlocks(name)
      env = env.add(name0, funType0)
      imir.FunctionHeader(linkage, visibility, cconv, retAttrs, name0, funType0, section, align, gc)
    }
  }


  def clFunctionDef(x:llvm.FunctionDef) = x match {
    case llvm.FunctionDef (funProto, blocks) => {
      val funProto0 = clFunctionHeader(funProto)
      val entryGraph = clEntryNode(blocks.head)
      //val (entryBlock, entryGraph) = clEntryBlock(blocks.head)
      val g = blocks.foldLeft(entryGraph) { (p,b) =>
        val g0 = clBlock(b)
        val g1 = p.splice[C](g0)
        g1
      }
      val (start, rest) = g.postorderDfs
      logger.debug("postorderDfs of {}", funProto0.name.pureImg)
      logger.debug("{}", rest.foldLeft("")((p,e) => p + "," + e.blockId))

      val (start1, rest1) = g.preorderDfs
      logger.debug("preorderDfs of {}", funProto0.name.pureImg)
      logger.debug("{}", rest1.foldLeft("")((p,e) => p + "," + e.blockId))

      logger.info("code")
      logger.info("{}", g.toInstStr)
      val g1 = g.map[imir.AbsNode](x=>x.frontBiasBlock)
      val g2 = g1.map[imir.AbsNode](x=>x.backBiasBlock)
      val g3 = g2.orderMap(g2.postorderDfs, (x=>x.frontBiasBlock))
      val g4 = g3.preorderDfs
      logger.info("graph {}", g)
      //logger.info("domtree.dot {}", DominatorTree.tree2dot(dt.asInstanceOf[DominatorTree]))
      imir.FunctionDef(env, funProto0, g)
    }
  }

  def clDbg(x:llvm.Dbg) = x match { case llvm.Dbg(s, meta) => imir.Dbg(s, clMetaConst(meta)) }
  def clModule() = {
    m match {
      case llvm.Module(list) => {
        val list1 = list.map(clTopLevel)
        imir.Module(list1, env)
      }
    }
  }
}