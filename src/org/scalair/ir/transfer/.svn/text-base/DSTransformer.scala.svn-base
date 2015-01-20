package org.scalair.ir.transfer

/**
 * User: wangn
 * Date: 4/16/11
 */



final case class DSTransformer (m:Module) {
  import Constants._


  val env = new Env (m) //m.defs)
  val util = new Util(env)
  import env._
  import util._



  def rewriteAlloca(lhs:Option[VarOrID], ty:Type, size:Option[Value], align:Option[Align]):InstSeq[AbsInst] = {
    val (inst0, t, s, indices) = size match {
      case Some(s) => {
        val (inst0, len0, x) = util.newAllocaSize(s, ty)
        (inst0, ty, Some(len0), x)
      }
      case None => {
        val (t, x) = util.newAllocaType(ty)
        (List(), t, None, x)
      }
    }
    val (inst1, lhs1) = util.allocaInst(t, s, align)
    val (inst2, ptr_dsect) = util.getElemPtrInst(lhs1, indices)
    val (inst3, base) = util.castInst(PtrtoInt, ptr_dsect, ptrAsIntType)
    //lhs.map(env.addReplace(_, ptr_dsect))
    val (inst30, bSize) = bufSize(ty, size)
    val inst4 = lhs.map(util.saveHSECT(_, base, bSize)).getOrElse(List())
    val (inst5, ptr_hsect) = util.computeHSECTSizeAddr(base)
    val (inst6, (m1, m2)) = util.computeMSECTAddrs(base, bSize, charPtrTy)
    util.registerMetadata(ptr_dsect, RtMetaData(env.curStackPType, base))
    val (inst7) = util.copyInst(lhs, ptr_dsect)
    InstSeq(cmt("newAllocSize", inst0):::
      cmt("alloca H+D+2M", inst1):::
      cmt("compute addr of D", inst2):::
      cmt("save buffer size", inst3:::inst30):::
      inst4:::
      inst5:::
      cmt("compute MSECT addrs", inst6):::
      cmt("copy ptr_desct to " + lhs, inst7))
  }

  def dummyPPInfo = PrimitiveConst(i32, IntConst("0"))

  def rewriteLoad(lhs:Option[VarOrID], v:Boolean, ptr:Value, align:Option[Align]):InstSeq[AbsInst] = {
    val ptrType = env.typeOf(ptr).asInstanceOf[PointerType]
    val elmType = ptrType.baseType
    val accSize = util.getTypeSizeInByte(elmType)
    val triplePtr = util.computeValueMetadata(ptr)
    val (inst0, ptrToI) = util.castInst(PtrtoInt, triplePtr.dataVal, ptrAsIntType)
    val inst1 = util.insertBoundCheck(triplePtr.rtMetaData.typeCode, ptrToI, accSize,
      triplePtr.rtMetaData.baseAddr, None, dummyPPInfo)

    val (inst2, bufSize) = util.loadSize(triplePtr.rtMetaData.baseAddr)
    val (inst3, rtmd) = util.loadMetadata(ptrToI, bufSize, elmType)
    util.registerMetadata(lhs, rtmd)
    InstSeq(inst0:::inst1:::inst2:::inst3:::List(Instruction(lhs, Load(v, ptr, align))))
  }

  def rewriteStore(vol:Boolean, v:Value, ptr:Value, align:Option[Align]):InstSeq[AbsInst] = {
    val accSize = util.castBufSize(util.getAccSize(ptr))
    val triplePtr = util.computeValueMetadata(ptr)
    val (inst0, ptrToI) = util.castInst(PtrtoInt, triplePtr.dataVal, ptrAsIntType)
    val inst1 = util.insertBoundCheck(triplePtr.rtMetaData.typeCode, ptrToI, accSize,
      triplePtr.rtMetaData.baseAddr, None, dummyPPInfo)

    val tripleV = util.computeValueMetadata(v)
    val (inst2, bSize) = loadSize(triplePtr.rtMetaData.baseAddr)
    val inst3 = storeMetadata(1, triplePtr.dataVal, bSize, tripleV.rtMetaData)
    val inst4 = util.storeValue(tripleV.dataVal, triplePtr.dataVal, vol, align)
    InstSeq(inst0:::inst1:::inst2:::inst3:::inst4)
  }

  def rewriteCastExpr(lhs:Option[VarOrID], rhs:CastExpr):InstSeq[AbsInst] = {
    rhs.op match {
      case Bitcast => {
        val tripleV = util.computeValueMetadata(rhs.tv)
        util.registerMetadata(lhs, tripleV.rtMetaData)
      }
      case _ => throw new Exception("abc")
    }
    InstSeq(List(Instruction(lhs, rhs)))
  }


  def rewriteGetElemPtr(lhs:Option[VarOrID], rhs:GetElemPtr):InstSeq[AbsInst] = {
    val triplePtr = computeValueMetadata(rhs.tv)
    registerMetadata(lhs, triplePtr.rtMetaData)
    InstSeq(List(Instruction(lhs, GetElemPtr(triplePtr.dataVal, rhs.indices))))
  }

  def rewriteCall(lhs:Option[VarOrID], cconv:Option[CConv], retAttrs:List[Attr], retType:Type,
                  unUMT:VarOrID, actualParams:ActualParamList, funAttrs:List[Attr]):InstSeq[AbsInst] = {
    val fnType = env.typeOf(unUMT).asInstanceOf[FunType];
    //val (inst0, rv0) = addStaticCallSiteSig(fnType.retType, actualParams, dummyPPInfo)
    val (inst1, fmPtr, actualParams1) = processActualParamList(lhs.map(env.typeOf(_)), actualParams)
    val inst2 = lhs match {
      case Some(l) => loadReturnMeta(l, fmPtr)
      case None => List()
    }
    InstSeq(inst1:::List(Instruction(lhs, Call(cconv, retAttrs, retType, unUMT, actualParams1, funAttrs))):::inst2)
  }


  def clAbsInst(x:AbsInst):List[AbsInst] = x match {
    case y:Instruction => clInstruction(y)
    case y => List(y)
  }

  def clInstruction(x:Instruction) = {
    val dse = x.rhs match {
      case Alloca(ty, size, align) => Some(rewriteAlloca(x.lhs, ty, size, align))
      case ArithmaticExpr(binop, carray, t, v1, v2) => None
      case BitwiseExpr(binop, t, v1, v2) => None
      case Call(cconv, retAttrs, retType, unUMT, actualParams, funAttrs) =>
        Some(rewriteCall(x.lhs, cconv, retAttrs, retType, unUMT, actualParams, funAttrs))
      case rhs:CastExpr => Some(rewriteCastExpr(x.lhs, rhs))
      case y:Extract => None
      case y:ExtractElement => None
      case y:ExtractValue => None
      case y:FCmpExpr => None
      case Free(tv) => None
      case rhs:GetElemPtr => Some(rewriteGetElemPtr(x.lhs, rhs))
      case y:GetResult => None
      case y:ICmpExpr => None
      case y:InsertElement => None
      case y:InsertValue => None
      case Load(v,ptr,align) => Some(rewriteLoad(x.lhs, v,ptr,align))
      case Malloc(ty, size, align) => None
      case y:Phi => None
      case y:Select => None
      case y:ShuffleVector => None
      case Store(volatile, v, ptr, align) => Some(rewriteStore(volatile, v, ptr, align))
      case y:VaArg => None
    }
    dse match {
      case Some(v) => Comment(x)::v.list
      case None => List(x)
    }
  }

  def clControlInst(x:ControlInst):(List[AbsInst], ControlInst) = x match {
    case Unreachable() => (List(), x)
    case Ret (v) => (List(), x)
    case Br(l) => (List(), x)
    case CBr(cond, tru, fal) => (List(), x)
    case IndirectBr(cond, branchs) => (List(), x)
    case Switch(v, default, jumptable) => (List(), x)
    case Invoke(lhs, cconv, retAttrs, unUMT, actualParams, funAttrs, label, exception) => (List(), x)
    case Unwind() => (List(), x)
  }

  def clControlInstDbg(x:ControlInstDbg):ControlInstDbg = x match {
    case ControlInstDbg(inst, dbg) => ControlInstDbg(clControlInst(inst)._2, dbg)
  }

  def procEntryBlock(prelog:List[AbsInst], x:Block):Block = x match {
    case Block(label, index, middle, end) => Block(label, index,
      prelog:::(middle.flatMap(clAbsInst)), clControlInstDbg(end))
  }

  def clBlock(x:Block) = x match {
    case Block(label, index, middle, end) => Block(label, index, middle.flatMap(clAbsInst), clControlInstDbg(end))
  }

  def clFormalParam(x:FormalParam) = x match {
    case FormalParam(ty, attr1, align, id, attr2) => FormalParam(ty, attr1, align, id, attr2)
  }

  def clFormalParamList(x:FormalParamList) =
    FormalParamList(x.list.map(clFormalParam(_)), x.hasDots, x.funAttrs)



  def clActualParam(x:ActualParam) = x match {
    case ActualParam(attrs1, align, v, attrs2) => ActualParam(attrs1, align, v, attrs2)
  }

  def clActualParamList(x:ActualParamList) = x match {
    case ActualParamList(list) => ActualParamList(list.map(clActualParam))
  }

  def clTopLevel(x:TopLevel) = x match {
    case Target(k, v) => Target(k, v)
    case Declare(header) => Declare(clFunctionHeader(header)._1)
    case TypeDef(name, ty) => TypeDef(name, ty)
    case UnamedType(id, ty) => UnamedType(id, ty)
    case DepLibs(l) => DepLibs(l)
    case ModuleAsm(str) => ModuleAsm(str)
    case x:Alias => clAlias(x)
    case x:Global => clGlobal(x)
    case StandardaloneMD(lhs, ty) => StandardaloneMD(lhs, ty)
    case NamedMD(lhs, rhs) => NamedMD(lhs, rhs)
    case x:DbgInit => clDbgInit(x)
    case x:FunctionDef => clFunctionDef(x)
  }

  def clAliasee(x:Aliasee) = x match {
    case AliaseeTV(tv) => AliaseeTV(tv)
    case AliaseeBitCast(tv, t) => AliaseeBitCast(tv, t)
    case AliaseeGetElemPtr(get) => AliaseeGetElemPtr(get)
  }


  def clAlias(x:Alias) = x match {
    case Alias(lhs, visibility, linkage, aliasee) => Alias(lhs, visibility, linkage, clAliasee(aliasee))
  }

  def clGlobal(x:Global) = x match {
    case Global(lhs, linkage, visiblilty, threadLocation, addrSpace, globalType, ty, const, section, align) => {
      val nc = const match {
        case None => None
        case Some(c) => Some(c)
      }
      Global(lhs, linkage, visiblilty, threadLocation,  addrSpace, globalType, ty, nc, section, align)
    }
  }

  def clStandardaloneMD(x:StandardaloneMD) = x match {
    case StandardaloneMD(lhs, ty) => StandardaloneMD(lhs, ty)
  }

  def clNamedMD(x:NamedMD) = x match {
    case NamedMD(lhs, rhs) => NamedMD(lhs, rhs)
  }

  def clDbgInit(x:DbgInit) = x match {
    case DbgInit(lhs, start) => DbgInit(lhs, start)
  }


  def clFunType(ft:FunType) = {
    val (l, fp) = processFormalParamList(ft.formalParams)
    (FunType(ft.retType, fp, ft.funAttrs), l)
  }

  def clFunctionHeader(x:FunctionHeader) = x match
  { case FunctionHeader (linkage, visibility, cconv, retAttrs, name, funType, section, align, gc) => {
      val (ft, l) = clFunType(funType);
      (FunctionHeader(linkage, visibility, cconv, retAttrs, name, ft, section, align, gc), l)
    }
  }

  def clFunctionDef(x:FunctionDef) = x match {
    case FunctionDef(funProto, blocks, g) => {
      val (hd, prelog) = clFunctionHeader(funProto)
      FunctionDef(hd, procEntryBlock(prelog, blocks.head)::blocks.tail.map(clBlock), g)
    }
  }

  def clModule() = m match {
    case Module(list, _) => Module(list.map(clTopLevel), env)
  }
}