package org.scalair.ir.imir

/**
 * User: wangn
 * Date: 4/20/11
 */

class Util(val defTab:Env) {
  import Constants._
  import org.scalair.ir.common._
  // data representation in bits

  import defTab._
  val env = defTab


  def cmt[T<:AbsInst](s:String, l:List[T]):List[AbsInst] =
    if (l.length > 0)
      (Comment(s)::l)
    else
      List()

  def bits(d:DataLayout, t:Type) = t match {
    case x0:Label => d.matrics.get("p").map(_.size)
    case x0:PointerType => d.matrics.get("p").map(_.size)
    case x0:PrimitiveType => Some(x0.bits)
    case _ => None
  }

  def getTypeCode(tc:Const):Const = tc match {
    case PrimitiveConst(t, Undef()) => PrimitiveConst(t, Zero())
    case PrimitiveConst(t, NullValue()) => PrimitiveConst(I(32), NullValue())
    case PrimitiveConst(I(n),_) => {
      if (n<=64)
        dtsTypeCodeIX(n)
      else
        throw new Exception ("unexpected case " + tc)
    }
    case PrimitiveConst(F(32),_) => dtsTypeCodeF32
    case PrimitiveConst(F(64),_) => dtsTypeCodeF64
    case PrimitiveConst(FloatType(),_) => dtsTypeCodeF32
    case PrimitiveConst(DoubleType(),_) => dtsTypeCodeF64
    case GlobalAddr(g) => dtsTypeCodeGP
    case StructConst(f, isPacked) => StructConst(f.map(getTypeCode), isPacked)
    case VectorConst(f) => VectorConst(f.map(getTypeCode))
    case ArrayConst(e) => ArrayConst(e.map(getTypeCode))
    case StrConst(str) => dtsTypeCodeGP
    case LabelQuoteStr(str) => dtsTypeCodeLP
    case BlockAddress(a1, a2) => dtsTypeCodeLP
    case _ => throw new Exception("unexpected case " + tc)
  }

  def getBaseAddr(tc:Const):Const = tc match {
    case PrimitiveConst(t, Undef()) => PrimitiveConst(t, Zero())
    case PrimitiveConst(t, NullValue()) => PrimitiveConst(I(32), NullValue())
    case PrimitiveConst(I(n),_) => {
      if (n<=64)
        dtsTypeCodeIX(n)
      else
        throw new Exception ("unexpected case " + tc)
    }
    case PrimitiveConst(F(32),_) => dtsTypeCodeF32
    case PrimitiveConst(F(64),_) => dtsTypeCodeF64
    case PrimitiveConst(FloatType(),_) => dtsTypeCodeF32
    case PrimitiveConst(DoubleType(),_) => dtsTypeCodeF64
    case GlobalAddr(g) => dtsTypeCodeGP
    case StructConst(f, isPacked) => StructConst(f.map(getTypeCode), isPacked)
    case VectorConst(f) => VectorConst(f.map(getTypeCode))
    case ArrayConst(e) => ArrayConst(e.map(getTypeCode))
    case StrConst(str) => dtsTypeCodeGP
    case LabelQuoteStr(str) => dtsTypeCodeLP
    case BlockAddress(a1, a2) => dtsTypeCodeLP
    case _ => throw new Exception("unexpected case " + tc)
  }

  def getBaseAddr(t:Type):Const = {
    getTypeSizeInBit(t) match {
      case x if x <= 64 => dtsTypeCodeIX(x)
      case 96 => ArrayConst(List(dtsTypeCodeB32,dtsTypeCodeB32, dtsTypeCodeB32))
      case 128 => ArrayConst(List(dtsTypeCodeB32,dtsTypeCodeB32, dtsTypeCodeB32, dtsTypeCodeB32))
      case 192 => ArrayConst(List(dtsTypeCodeB32,dtsTypeCodeB32, dtsTypeCodeB32, dtsTypeCodeB32,dtsTypeCodeB32,dtsTypeCodeB32))
      case  _ => throw new Exception("getBaseAddr " + t)
    }
  }

  def getNeWType(t:Type):Type =
    StructType(List(i32, i32, t, getMetaType(t), getMetaType(t)))


  def getTypeCode(t:Type):Const = t match {
    case x:I => {
      if (x.bits<=64) dtsTypeCodeIX(x.bits)
      else x.bits match {
        case 96 => ArrayConst(List(dtsTypeCodeB32,dtsTypeCodeB32, dtsTypeCodeB32))
        case 128 => ArrayConst(List(dtsTypeCodeB32,dtsTypeCodeB32, dtsTypeCodeB32, dtsTypeCodeB32))
        case 192 => ArrayConst(List(dtsTypeCodeB32,dtsTypeCodeB32, dtsTypeCodeB32, dtsTypeCodeB32,dtsTypeCodeB32,dtsTypeCodeB32))
        case  _ => throw new Exception("getTypeCode " + t)
      }
    }
    case x:FloatingType => {
      x.bits match {
        case 32 => dtsTypeCodeF32
        case 64 => dtsTypeCodeF64
        case 80 => dtsTypeCodeF80
        case 128 => ArrayConst(List(dtsTypeCodeF64, dtsTypeCodeF64))
        case _ => throw new Exception("bad " + t)
      }
    }
    case _ => throw new Exception("bad " + t)
  }

  def makeCStdCall(f:VarOrID, params:List[Value]) = {
    val retType = VoidType() //env.typeOf(f).asInstanceOf[FunType].retType
    Call(None, List(), retType, f, ActualParamList(params.map(v => ActualParam(List(), None, v, List()))), List())
  }

  def insertCallCheck(ptr:Value, dtsTypeCode:Value, calleeSig:Value, callsiteSig:Value, currPPInfo:Value) = {
    val c = makeCStdCall(funCheckCall, List(ptr, dtsTypeCode, calleeSig, callsiteSig, currPPInfo))
    Instruction(None, c, List())
  }

  def insertBoundCheck(dtsTypeCode:Value, ptr:Value, accSize:Value, bufBase:Value, bufSize:Option[Value], currPPInfo:Const):List[AbsInst] = {
    val c = bufSize match {
      case Some(s) => makeCStdCall(dtsFunCheckBoundsExt, List(ptr, accSize, dtsTypeCode, bufBase, currPPInfo))
      case None => makeCStdCall(dtsFunCheckBounds, List(ptr, accSize, dtsTypeCode, currPPInfo))
    }
    List(Instruction(None, c, List()))
  }

  def getLogSwitchs(x:Int):Const = { PrimitiveConst(i32, zero) }

  def getStringStoragePtr(s:String):Const = { PrimitiveConst(i32, zero) }

  val castInstCache = CommonExpCache[CastExpr, VarOrID]()
  def castInst(op:ConvOp, ptr:Value, dest:Type):(List[AbsInst], Value) = {
    val srcTy = env.typeOf(ptr)
    if (srcTy == dest) {
      (List(), ptr)
    } else {
      val rhs = CastExpr(op, ptr, dest)
      castInstCache.get(rhs) match {
        case Some(x) => (List(), x)
        case None => {
          val lhs = defTab.getNewLocalVar("cast", dest)
          castInstCache.add(rhs, lhs)
          (List(Instruction(Some(lhs), rhs, List())), lhs)
        }
      }
    }
  }

  def copyInst(lhs:Option[VarOrID], rhs:Value):(List[AbsInst]) = {
    val t = env.typeOf(rhs)
    val rtMetaData = ptrToRtMetaData.get(rhs) match {
      case Some(x) => registerMetadata(lhs, x)
      case None => throw new Exception ("bad")
    }
    List(Instruction(lhs, CastExpr(Bitcast, rhs, t)))
  }


  val getElemPtrInstCache = CommonExpCache[GetElemPtr, VarOrID]()
  def getElemPtrInst(addr:Value, indices:List[Const]):(List[AbsInst], VarOrID) = {
    val rhs = GetElemPtr(addr, indices)
    getElemPtrInstCache.get(rhs) match {
      case Some(x) => (List(), x)
      case None => {
        val lhs = defTab.getNewLocalVar("gep", env.typeOf(GetElemPtr(addr, indices)))
        getElemPtrInstCache.add(rhs, lhs)
        (List(Instruction(Some(lhs), rhs, List())), lhs)
      }
    }
  }

  val arithInstCache = CommonExpCache[ArithmaticExpr, VarOrID]()
  def arithInst(binop:ArithmaticOp, carry:List[Carry], v1:Value, v2:Value):(List[AbsInst], VarOrID) = {
    val rhs = ArithmaticExpr(binop, carry, env.typeOf(v1), v1, v2)
    arithInstCache.get(rhs) match {
      case Some(x) => (List(), x)
      case None => {
        val lhs = defTab.getNewLocalVar("arith", env.typeOf(rhs))
        arithInstCache.add(rhs, lhs)
        (List(Instruction(Some(lhs), rhs, List())), lhs)
      }
    }
  }


  def makeAddrPrinter(defTab:Env, dl:Int, msg:String, ptr:Value, pushCurDTSSym:Boolean, currPPInfo:Const) = {
    val l =
      if (pushCurDTSSym) List(getLogSwitchs(dl), currPPInfo, getStringStoragePtr(msg))
      else List(getLogSwitchs(dl), getStringStoragePtr(msg))

    val (cast, lhs) = castInst(PtrtoInt, ptr, ptrAsIntType)
    val call = makeCStdCall(if (pushCurDTSSym) funLogPPAddr else funLogAddr, l:::List(lhs))
    List(cast, Instruction(None, call, List()))
  }

  def makeMsgPrinter(defTab:Env, dl:Int, msg:String, pushCurDTSSym:Boolean, currPPInfo:Const) = {
    val l =
      if (pushCurDTSSym) List(getLogSwitchs(dl), currPPInfo, getStringStoragePtr(msg))
      else List(getLogSwitchs(dl), getStringStoragePtr(msg))

    val call = makeCStdCall(if (pushCurDTSSym) funLogPPMsg else funLogMsg, l)
    List(Instruction(None, call, List()))
  }

  def makeValPrinter(defTab:Env, dl:Int, msg:String, size:Value, value:Value, currPPInfo:Option[Const]) = {
    val l = currPPInfo match {
      case Some(x) => List(getLogSwitchs(dl), x, getStringStoragePtr(msg), size, value)
      case None => List(getLogSwitchs(dl), getStringStoragePtr(msg), size, value)
    }
    val call = makeCStdCall(if(currPPInfo.isDefined) funLogPPVal else funLogVal, l)
    List(Instruction(None, call, List()))
  }


  def makeSrcInfoPrinter(dl:Int, msg:String, coord:Const) = {
    val l = List(getLogSwitchs(dl), coord)
    val call = makeCStdCall(funLogSrcInfo, l)
    List(Instruction(None, call, List()))
  }

  def makeLogPPSymAndMsgPrinter(dl:Int, msg:String, coord:Const) = {
    val l = List(getLogSwitchs(dl), coord, getStringStoragePtr(msg))
    val call = makeCStdCall(funLogPPSymAndMsg, l)
    List(Instruction(None, call, List()))
  }

  def makeDynamicPPInfoPrinter(dl:Int, msg:String, coord:Const) = {
    val l = List(coord, getStringStoragePtr(msg))
    val call = makeCStdCall(funPrintDynamicPPInfo, l)
    List(Instruction(None, call, List()))
  }

  val msectAddrs = CommonExpCache[Value, (Value, Value)]
  def computeMSECTAddrs(addr:Value, offset:Value, ptrType:PointerType):(List[AbsInst], (Value, Value)) = {
    msectAddrs.get(addr) match {
      case Some(x) => (List(), x)
      case None => {
        val (addrInt, l) = env.typeOf(addr) match {
          case x:PointerType => {
            val (cast, lhs) = castInst(PtrtoInt, addr, ptrAsIntType)
            (lhs, cast)
          }
          case  _ => (addr, List())
        }

        val (inst1, m1i) = arithInst(Add, List(Nsw), addrInt, offset)
        val (inst2, m2i) = arithInst(Add, List(Nsw), m1i, offset)
        val (inst3, m1ip) = castInst(InttoPtr, m1i, ptrType)
        val (inst4, m2ip) = castInst(InttoPtr, m2i, ptrType)
        msectAddrs.add(addr, (m1ip, m2ip))
        (cmt("computeMSECTAddrs of " + addr + " offset " + offset, l:::inst1:::inst2:::inst3:::inst4), (m1ip, m2ip))
      }
    }
  }


  def castPointer(addr:Value, t:PointerType):(List[AbsInst], Value) = {
    env.typeOf(addr) match {
      case t0:PointerType => if (t0 != t) castInst(Bitcast, addr, t) else (List(), addr)
      case _ => throw new Exception ("bad")
    }
  }

  def storeValue(v:Value, addr:Value, volatile:Boolean = false, align:Option[Align]=None):List[AbsInst] = {
    def storeInst(v:Value, addr:Value):List[AbsInst] = List(Instruction(None, Store(volatile, v, addr, align)))

    def storeAggregateValue(addr:Value, l:List[Value]):List[AbsInst] = {
      val (lx, _) = l.foldLeft((List[AbsInst](), 0)) ((p, e) => {
        val indices = List(PrimitiveConst(i32, zero), PrimitiveConst(i32, IntConst(p._2.toString)))
        val (inst1, r1) = getElemPtrInst(addr, indices)
        val inst2 = storeValue(e, r1)
        (p._1:::inst1:::inst2, p._2 + 1)
      })
      lx
    }
    val (inst0, naddr) = castPointer(addr, PointerType(env.typeOf(v)))
    val inst1 = v match {
      case StructConst(fields,_) => storeAggregateValue(naddr, fields)
      case StructValue(fields,_) => storeAggregateValue(naddr, fields)
      case ArrayConst(fields) => storeAggregateValue(naddr, fields)
      case ArrayValue(fields) => storeAggregateValue(naddr, fields)
      case _ => storeInst(v, naddr)
    }
    inst0:::inst1
  }

  def loadValue(addr:Value):(List[AbsInst], Value) =
    env.typeOf(addr) match {
      case PointerType(bt, _) => loadValue(addr, bt)
      case x => throw new Exception ("bad type" + x)
    }

  def loadValue(addr:Value, te:Type, volatile:Boolean=false, align:Option[Align]=None):(List[AbsInst], Value) = {
    def loadInst(r1:Value, te:Type, volatile:Boolean, align:Option[Align]):(List[AbsInst], VarOrID) = {
      val lhs = defTab.getNewLocalVar("ld", te)
      val inst2 = Instruction(Some(lhs), Load(volatile, r1, align))
      (List(inst2), lhs)
    }

    def load(addr:Value, n:Int):(List[AbsInst], Value) = {
      val indices = List(PrimitiveConst(i32, zero), PrimitiveConst(i32, IntConst(n.toString)))
      val (inst1, a1) = getElemPtrInst(addr, indices)
      val (inst2, v) = loadValue(a1, env.typeOf(a1))
      (inst1:::inst2, v)
    }

    val (inst0, naddr) = castPointer(addr, PointerType(te))
    val (inst1, v) = te match {
      case ArrayType(n,e) =>  {
        val l0 = for (i <- 0 to n) yield load(naddr, i)
        val (insts, vals) = l0.foldLeft(List[AbsInst](), List[Value]()) ((p,e) => (p._1:::e._1, p._2:::List(e._2)))
        (insts, ArrayValue(vals))
      }
      case StructType(l,isPacked) => {
        val (insts, vals, _) = l.foldLeft((List[AbsInst](), List[Value](), 0)) ((p, e) => {
          val (inst0, lhs) = load(naddr, p._3)
          (p._1:::inst0, p._2:::List(lhs), p._3 + 1)
        })
        (insts, StructValue(vals, isPacked))
      }
      case x:PointerType => loadInst(naddr, te, volatile, align)
      case x:PrimitiveType => loadInst(naddr, te, volatile, align)
      case _ => throw new Exception("unexpected type " + te)
    }
    (inst0:::inst1, v)
  }

  def storeMetadata(dl:Int, addr:Value, offset:Value, rtMetaData:RtMetaData):List[AbsInst] = {
    val dtsTypeCode = rtMetaData.typeCode
    val base = rtMetaData.baseAddr
    val ptrType = PointerType(env.typeOf(dtsTypeCode))
    val (l0, (m1, m2)) = computeMSECTAddrs(addr, offset, ptrType)
    val l1 = storeValue(dtsTypeCode, m1)
    val l2 = storeValue(base, m2)
    cmt("storeMetadata", l0:::l1:::l2)
  }

  def computePPInfoAddr(in:Value):(List[AbsInst], Value) = {
    val  (inst1, lhs) = arithInst(Add, List(Nsw), in, constHSectSymbOffset)
    val (inst2, hsectAddr) = castInst(InttoPtr, lhs, PointerType(ptrOfStructPPInfoType))
    (inst1:::inst2, hsectAddr)
  }

  val hsectSizeAddrCache = CommonExpCache[Value, Value]()
  def computeHSECTSizeAddr(i:Value):(List[AbsInst],Value) = {
    hsectSizeAddrCache.get(i) match {
      case Some(x) => (List(), x)
      case None => {
        val (inst0, iv) = env.typeOf(i) match {
          case x:PointerType => val (inst, x) = castInst(PtrtoInt, i, ptrAsIntType); (inst, x)
          case _ => (List(), i)
        }
        val (inst1, hsectAddInt) = arithInst(Add, List(Nsw), i, constHSectSizeOffset)
        val (inst2, hsectAddr) = castInst(InttoPtr, hsectAddInt, sizePtrType)
        hsectSizeAddrCache.add(i, hsectAddr)
        (cmt("computeHSECTSizeAddr of " + i, inst0:::inst1:::inst2), hsectAddr)
      }
    }
  }

  def loadPPInfo(base:Value):(List[AbsInst], Value) = {
    val (inst1, baseInt) = castInst(PtrtoInt, base, ptrAsIntType)
    val (inst2s, bci) = computePPInfoAddr(baseInt)
    val (inst3, si) = loadValue(bci)
    (inst1:::inst2s:::inst3, si)
  }

  def loadSize(base:Value):(List[AbsInst], Value) = {
    val (inst0, hsectAddr) = computeHSECTSizeAddr(base)
    val (inst1, size) = loadValue(hsectAddr)
    (inst0:::inst1, size)
  }

  def loadSize(ptr:Value, base:Value):(List[AbsInst], Value) = {
    val (insts, size) = ptrToSizeLoc.get(ptr) match {
      case Some(loc) => loadValue(loc)
      case None => loadSize(base)
    }
    ptrToSize += (ptr -> size)
    (insts, size)
  }


  def allocaInst(t:Type, s:Option[Value]=None, align:Option[Align]=None):(List[AbsInst], LocalVarOrID) = {
    val lhs = defTab.getNewLocalVar("alloc", PointerType(t))
    (List(Instruction(Some(lhs), Alloca(t, s, align))), lhs)
  }

  def addEpilog() = {
    val c = makeCStdCall(popStackPType, List())
    Instruction(None, c)
  }

  def initDynamicPPInfo(storage:Value, srcInfo:Value, name:String, prev:Value):List[AbsInst] = {
    val (inst0, srcInfoAddr) = getElemPtrInst(storage, zeroZero)
    val inst1 = storeValue(srcInfo, srcInfoAddr)
    val (inst2, nameAddr) = getElemPtrInst(storage, zeroOne)
    val inst3 = storeValue(getStringStoragePtr(name), nameAddr)
    val (inst4, prevAddr) = getElemPtrInst(storage, zeroTwo)
    val inst5 = storeValue(prev, prevAddr)
    inst0:::inst1:::inst2:::inst3:::inst4:::inst5
  }

  def getNullValue(t:PointerType) = PrimitiveConst(t, NullValue())



  def allocStaticFileInfo(dir:String, file:String):Const = {
    val key = dir + "/" + file
    strToFileInfo.get(key) match {
      case Some(x) => x
      case None => {
        val cdir = getStringStoragePtr(dir)
        val cfile = getStringStoragePtr(file)
        val const_struct_6 = StructConst(List(cdir, cfile), false)
        strToFileInfo += (key -> const_struct_6)
        const_struct_6
      }
    }
  }

  lazy val nullPPInfo = getNullValue(PointerType(structPPInfoType))

  def getIntConstant(n:Int):Const =  PrimitiveConst(i32, IntConst(n.toString))

  def allocStaticSrcInfo(dir:String, file:String, line:Int, col:Int):Const = {
    val key = dir + "/" + file + line + ":" + col
    strToCoord.get(key) match {
      case Some(x) => x
      case None => {
        val fileInfo = allocStaticFileInfo(dir, file)
        val cline = getIntConstant(line)
        val ccol = getIntConstant(col)
        val const_struct_6 = StructConst(List(fileInfo, cline, ccol), false)
        val tc = const_struct_6
        strToCoord += (key -> tc)
        tc
      }
    }
  }

  def allocStaticPPInfo(srcInfo:Const, name:String):Const = {
    val cname = getStringStoragePtr(name)
    val const_struct_6 = StructConst(List(srcInfo, cname, nullPPInfo), false)
    const_struct_6
  }

  def getMetaType(ty:Type):Type = ty match {
    case x:PrimitiveType => env.typeOf(getTypeCode(x))
    case ArrayType(n, b) => ArrayType(n, getMetaType(b))
    case VectorType(n, b) => VectorType(n, getMetaType(b))
    case StructType(lst, isPacked) => StructType(lst.map(getMetaType), isPacked)
    case _ => ty
  }

  def newAllocaType(ty:Type, isPacked:Boolean=false):(Type, List[Const]) = {
    (StructType(List(ArrayType(2, ptrAsIntType), ty, ArrayType(2, getMetaType(ty))), false), zeroOne)
  }

  def newAllocaSize(s:Value, ty:Type):(List[AbsInst], Value, List[Const]) = {
    // we need to optimize this later
    val (inst0, len0) = arithInst(Add, List(Nsw), getIntConstant(HSECT_LENGTH), s)
    (inst0, len0, List(getIntConstant(HSECT_LENGTH)))
  }

  var allocaToStaticPPInfoMap:Map[Value, Const] = Map()

  def getPPInfo(v:Value):Const = {
    allocaToStaticPPInfoMap.get(v) match {
      case Some(v) => v
      case None => {
        val coord = allocStaticSrcInfo("", "", 0, 0)
        val dtsSym = allocStaticPPInfo(coord, "")
        allocaToStaticPPInfoMap += (v -> dtsSym)
        dtsSym
      }
    }
  }

  def saveHSECT(origBase:Value, newBase:Value, size:Value):List[AbsInst] = {
    val (inst0, ptr_hsect) = computeHSECTSizeAddr(newBase)
    val inst1 = storeValue(size, ptr_hsect);
    val ppinfo = getPPInfo(origBase)
    val (inst3, bci) = computePPInfoAddr(newBase);
    val inst4 = storeValue(ppinfo, bci)
    cmt("save " + size + " to HSECT", inst0:::inst1)//:::
    //cmt("computePPInfoAddr", inst3):::
    //cmt("store ppinfo", inst4)
  }

  def getTypeSizeInBit(ty:Type):Int = ty match {
    case x:PrimitiveType => x.bits
    case x:PointerType => env.sizeOfInBit(x)
    case _ => throw new Exception("getTypeSizeInBit " + ty)
  }

  def getTypeSizeInByte(ty:Type):PrimitiveConst =
    PrimitiveConst(i32, SizeOf(ty))

/*
    ty match {
      case x:PrimitiveType => PrimitiveConst(i32, IntConst((x.bits/8).toString))
      case x:PointerType => PrimitiveConst(i32, IntConst((dataLayout.ptrSize/8).toString))
      case _ => PrimitiveConst(i32, SizeOf(ty))
    }
    */

  def allocaDSECTMSECTS(ty:Type, origBase:Value) = {
    val (nty, indices) = newAllocaType(ty, false)
    val (inst0, i1) = allocaInst(nty)
    val (inst1, ptr_dsect) = getElemPtrInst(i1, indices)
    val (inst2, pi) = castInst(PtrtoInt, ptr_dsect, ptrAsIntType)
    val const_int32_size = getTypeSizeInByte(ty)
    val inst3 = saveHSECT(origBase, pi, const_int32_size)
    val (inst4, ptr_hsect) = computeHSECTSizeAddr(pi)
    val dtsTypeCode = getTypeCode(env.typeOf(ptr_dsect))
    registerMetadata(ptr_dsect, RtMetaData(dtsTypeCode, pi))
    (cmt("allocaDSECTMSECTS", inst0:::inst1:::inst2:::inst3:::inst4),  ptr_dsect)
  }

  val nullSrcInfo = allocStaticSrcInfo("<null>", "<null>", 0, 0)


  def registerMetadata(ptr:Option[Value], rtMetaData:RtMetaData) {
    ptr match {
      case Some(p) => ptrToRtMetaData += (p -> rtMetaData)
      case None => ()
    }
  }

  def registerMetadata(ptr:Value, rtMetaData:RtMetaData) {
    ptrToRtMetaData += (ptr -> rtMetaData)
  }


  /*
  def storeReturnMeta(ret:Ret, savedFmDataPtr:Value):List[AbsInst] = {
    val (inst0, loaded) = loadValue(savedFmDataPtr)
    val inst1 = storeValue(loaded, dtsFunCallMetaDataPtr)
    val inst2 = ret.v match {
      case head::tail => {
        val edtsTypeCode = getTypeCode(env.typeOf(head))
        val ptrTyp = PointerType(env.typeOf(edtsTypeCode))
        val tripleV = computeValueMetadata(head)
        val (inst3, ptr_14) = loadValue(dtsFunCallMetaDataPtr)
        val (inst4, ptr_15) = getElemPtrInst(ptr_14, zeroZero)
        val (inst5, offset) = loadValue(ptr_15)
        val (inst6, retBaseLocAddr) = getElemPtrInst(ptr_14, zeroFive)
        val (inst7, retBaseAddr) = loadValue(retBaseLocAddr)
        val (inst8, retBaseAddrInt) = castInst(PtrtoInt, retBaseAddr, ptrAsIntType)
        val inst9 = storeMetadata(1, retBaseAddrInt, offset, tripleV.rtMetaData)
        inst3:::inst4:::inst5:::inst6:::inst7:::inst8:::inst9
      }
      case List() => {
        List()
      }
    }
    inst0:::inst1:::inst2
  }
  */

  def patchReturnMeta(ret:Ret) {

  }



  val HSECT_LENGTH = 8
  def computeDTSMemorySize(v:Value):(List[AbsInst], Value) = {
    val (inst0, size0) = arithInst(Mul, List(Nsw), v, getIntConstant(3))
    val (inst1, size1) = arithInst(Add, List(Nsw), size0, getIntConstant(HSECT_LENGTH))
    (inst0:::inst1, size1)
  }

  def computeDTSMemorySize(v:Const):Const =
    ConstArithmaticExpr(Add, List(),
      ConstArithmaticExpr(Mul, List(Nsw), v, getIntConstant(3)), getIntConstant(HSECT_LENGTH))

  def computeBaseAddr(memAddr:Value):(List[AbsInst], Value) = {
    val (inst0, intAddr) = castInst(PtrtoInt, memAddr, ptrAsIntType)
    val (inst1, baseAddr) = arithInst(Add, List(Nsw), intAddr, getIntConstant(HSECT_LENGTH))
    val (inst2, charAddr) = castInst(InttoPtr, baseAddr, charPtrTy)
    (inst0:::inst1:::inst2, charAddr)
  }

  def addStaticSig(name:String, ppinfo:Const, linkage:Linkage, retType:Type, types:List[Type], isVariadic:Boolean):(Global, Const) = {
    var fieldType = List(ptrOfStructPPInfoType, sizeIntType, intType, sizeIntType)
    val const_array_17_elemts = types.map(x => getTypeSizeInByte(x))

    val ArrayTy_2 = ArrayType(types.length, intType)
    val structTy = StructType(fieldType:::List(ArrayTy_2), false)
    val const_array_17 = ArrayConst(const_array_17_elemts)


    val const_struct_elems = List(ppinfo)
    val const_struct_elems0 =
      if (retType.isInstanceOf[VoidType])
        const_struct_elems:::List(getIntConstant(0))
      else
        const_struct_elems:::List(getTypeSizeInByte(retType))

    val const_struct_elems1 =
      if (isVariadic)
        const_struct_elems0:::List(getIntConstant(1))
      else
        const_struct_elems0:::List(getIntConstant(0))

    val const_struct_elems2 = const_struct_elems1:::List(getIntConstant(types.length), const_array_17)
    val const_struct = StructConst(const_struct_elems2)

    val gvar_array_ar = defTab.getNewGlobalVar(name, structTy)
    val global = Global(Some(gvar_array_ar), Some(linkage), None, false, None, GlobalType("constant"),
      structTy, Some(const_struct), None, None)
    (global, GlobalAddr(gvar_array_ar))
  }



  def addStaticFunSig(name:GlobalVarOrID, linkage:Linkage, funType:FunType) = {
    val v = getPPInfo(name)
    val ppInfo = ConstCast(InttoPtr, getIntConstant(0), ptrOfStructPPInfoType)
    val (global, gvar_array_ar) = addStaticSig(name.toString + "_sig",
      ppInfo, linkage, funType.retType, funType.formalParams.types, funType.formalParams.hasDots)
    funSigs += (name.toString -> gvar_array_ar)
  }

  def addStaticCallSiteSig(retType:Type, actualParams:ActualParamList, curPPInfo:Value):(List[AbsInst], Const) = {
    val ppInfo = ConstCast(InttoPtr, getIntConstant(0), ptrOfStructPPInfoType)
    val (global, gvar_array_ar) = addStaticSig("callSite", ppInfo, Private, retType, actualParams.values.map(env.typeOf(_)), false)
    val ppInfoAddr = ConstGetElemPtr(true, gvar_array_ar, zeroZero)
    val inst0 = storeValue(curPPInfo, ppInfoAddr)
    (inst0, ConstCast(PtrtoInt, gvar_array_ar, ptrAsIntType))
  }


  def ptrToIntInst(ptr:Value):(List[AbsInst], Value) = {
    val te = env.typeOf(ptr)
    if (te.isInstanceOf[I])
      (List(), ptr)
    else
      castInst(PtrtoInt, ptr, ptrAsIntType)
  }

  def intCastInst(v:Value, t:I):(List[AbsInst], Value) = {
    val vt = env.typeOf(v).asInstanceOf[I]
    (List(), v)
  }

  def checkBound(ptr:Value, accSize:Value, dtsTypeCode:Value, bufBase:Value, curPPInfo:Value):List[AbsInst] = {
    val (inst0, ptr0) = ptrToIntInst(ptr)
    val c = makeCStdCall(dtsFunCheckBounds, List(ptr0, accSize, dtsTypeCode, bufBase, curPPInfo))
    val inst1 = Instruction(None, c)
    inst0:::List(inst1)
  }

  def checkMemSet(dest:Value, d_type:Value, d_base:Value, len:Value, ppInfo:Value):List[AbsInst] = {
    val (inst0, dest0) = ptrToIntInst(dest)
    val c = makeCStdCall(dtsFunCheckMemset, List(dest0, d_type, d_base, len, ppInfo))
    inst0:::List(Instruction(None, c))
  }

  def checkMemFun(fun:VarOrID, dest:Value, d_type:Value, d_base:Value, src:Value, s_type:Value, s_base:Value, len:Value, ppInfo:Value):List[AbsInst] = {
    val (inst0, dest0) = ptrToIntInst(dest)
    val (inst1, src0) = ptrToIntInst(src)
    val c = makeCStdCall(fun, List(dest0, d_type, d_base, src0, s_type, s_base, len, ppInfo))
    inst0:::inst1:::List(Instruction(None, c))
  }


  def memfunc(fun:VarOrID, dest:Value, src:Value, size:Value, align:Const, volta:Const):List[AbsInst] = {
    val (inst0, dest0) = castPointer(dest, charPtrTy)
    val (inst1, src0) = castPointer(src, charPtrTy)
    val c = makeCStdCall(fun, List(dest0, src0, size, align, volta))
    inst0:::inst1:::List(Instruction(None, c))
  }

  def memcpy(dest:Value, src:Value, size:Value, align:Const, volta:Const):List[AbsInst] = {
    memfunc(llvmMemcpy, dest, src, size, align, volta)
  }

  def memset(dest:Value, size:Value, align:Const, volta:Const):List[AbsInst] = {
    val (inst0, dest0) = castPointer(dest, charPtrTy)
    val c = makeCStdCall(llvmMemset, List(dest0, size, align, volta))
    inst0:::List(Instruction(None, c))
  }


  def split[A,B](list:List[(A,B)]):(List[A], List[B]) =
    list.foldLeft(List[A](), List[B]()) ((p,e) => (p._1:::List(e._1), p._2:::List(e._2)))

  def computeConstMetadata(c:Const):(Const, Const) =
    c match {
      case PrimitiveConst(t, cl) => (getTypeCode(t), getTypeCode(t))
      case GlobalAddr(g) => (dtsTypeCodeGP, c)
      case StructConst(l,isPacked) => {
        val (l1, l2) = split[Const, Const](l.map(computeConstMetadata(_)))
        (StructConst(l1,isPacked), StructConst(l2, isPacked))
      }
      case ArrayConst(l) => {
        val (l1, l2) = split[Const,Const](l.map(computeConstMetadata(_)))
        (ArrayConst(l1), ArrayConst(l2))
      }
      case VectorConst(l) => {
        val (l1, l2) = split[Const, Const](l.map(computeConstMetadata(_)))
        (VectorConst(l1), VectorConst(l2))
      }
      case _ => throw new Exception("error")
    }

  def computeValueMetadata(v:Value):TripleValue = {
    val v0 = getReplacement(v)
    val rtMetaData = ptrToRtMetaData.get(v0) match {
      case Some(x) => x
      case None => {
        val (m1, m2) = v0 match {
          case x:Const => computeConstMetadata(x)
        }
        registerMetadata(v, RtMetaData(m1, m2))
        RtMetaData(m1, m2)
      }
    }
    TripleValue(v0, rtMetaData)
  }

  def saveSizeInHSECT(name:String, newBase:Value, size:Value):List[AbsInst] = {
    val (inst0, ptr_hsect) = computeHSECTSizeAddr(newBase);
    val inst1 = storeValue(size, ptr_hsect)
    inst0:::inst1
  }


  def loadMetadata(addr:Value, offset:Value, mty:Type):(List[AbsInst], RtMetaData) = {
    val bPair = getBaseAddr(mty)
    val basePtr = PointerType(env.typeOf(bPair))
    val (inst0, (m1, m2)) = computeMSECTAddrs(addr, offset, basePtr)
    val (inst1, ty) = loadValue(m1, env.typeOf(bPair))
    val (inst2, base) = loadValue(m2, env.typeOf(bPair))
    (inst0:::inst1:::inst2, RtMetaData(ty, base))
  }

  def castBufSize(v:Value) = v

  def getAccSize(ptr:Value):Const = {
    val pt = env.typeOf(ptr) match {
      case x:PointerType => x
      case t => throw new Exception("bad " + ptr + " is " + t)
    }
    val et = pt.baseType
    getTypeSizeInByte(et)
  }

  def bufSize(ty:Type, size:Option[Value]):(List[AbsInst], Value) = size match {
    case Some(s) => arithInst(Mul, List(Nsw), s, getTypeSizeInByte(ty))
    case None => (List(), getTypeSizeInByte(ty))
  }

  def dsAlloca(ty:Type, size:Option[Value]=None, align:Option[Align]=None):(List[AbsInst], VarOrID, Value) = {
    val (inst0, t, s, indices) = size match {
      case Some(s) => {
        val (inst0, len0, x) = newAllocaSize(s, ty)
        (inst0, ty, Some(len0), x)
      }
      case None => {
        val (t, x) = newAllocaType(ty)
        (List(), t, None, x)
      }
    }
    val (inst1, lhs1) = allocaInst(t, s, align)
    val (inst2, ptr_dsect) = getElemPtrInst(lhs1, indices)
    val (inst3, base) = castInst(PtrtoInt, ptr_dsect, ptrAsIntType)
    val (inst30, bSize) = bufSize(ty, size)
    val (inst5, ptr_hsect) = computeHSECTSizeAddr(base)
    val (inst6, (m1, m2)) = computeMSECTAddrs(base, bSize, charPtrTy)
    registerMetadata(ptr_dsect, RtMetaData(curStackPType, base))
    (cmt("newAllocSize", inst0):::
      cmt("alloca H+D+2M", inst1):::
      cmt("compute addr of D", inst2):::
      cmt("save buffer size", inst3:::inst30):::
      inst5:::
      cmt("compute MSECT addrs", inst6), ptr_dsect, bSize)
  }


  lazy val dtsActualParamType = StructType(
    List(
      sizeIntType, // number of actual parameters
      PointerType(ptrAsIntType), // addresses of actual parameters
      ptrAsIntType,  // the address of buf for actual parameters and their meta data
      ptrAsIntType,  // the address of buf for return value and its metadata
      ptrOfStructPPInfoType, // static ppinfo
      ptrOfStructPPInfoType // dynamic ppinfo for display call stack
    ), false)

  private lazy val ApRtMdparamNum = zeroZero
  private lazy val ApRtMdparamAddrIndices = zeroOne
  private lazy val ApRtMdparamBufAddr = zeroTwo
  private lazy val ApRtMdreturnBufAddr = zeroThree
  private lazy val ApRtMdstaticPPInfo = zeroFour
  private lazy val ApRtMddynamicPPInfo = zeroFive

  def processActualParamList(retType:Option[Type], actualParamList:ActualParamList):(List[AbsInst], VarOrID, ActualParamList) = {
    val (fieldList, tripleVList, list) = actualParamList.list.foldLeft((List[Type](), List[TripleValue](), List[ActualParam]())) ((p, n) => {
      val pTy = env.typeOf(n.v)
      val tv = computeValueMetadata(n.v)
      (p._1:::List(pTy), p._2:::List(tv), p._3:::List(ActualParam(n.attrs1, n.align, tv.dataVal, n.attrs2)))
    })
    val (insty0, fmPtr) = allocaInst(dtsActualParamType)
    val (insty1, numAddr) = getElemPtrInst(fmPtr, ApRtMdparamNum)
    val (insty2) = storeValue(getIntConstant(fieldList.length), numAddr)
    val (instx0, paramIndices) = allocaInst(ptrAsIntType, Some(getIntConstant(fieldList.length)))
    val (insty3, indexAddr) = getElemPtrInst(fmPtr, ApRtMdparamAddrIndices)
    val insty5 = storeValue(paramIndices, indexAddr)
    val sTy = StructType(fieldList)
    val (inst0, aiBase, bSize) = dsAlloca(sTy)
    val (insty10, aiBasePtr) = castInst(PtrtoInt, aiBase, ptrAsIntType)
    val (insty11, aiBaseStoreAddr) = getElemPtrInst(fmPtr, ApRtMdparamBufAddr)
    val (insty12) = storeValue(aiBasePtr, aiBaseStoreAddr)
    val (inst1,_) = tripleVList.foldLeft((List[AbsInst](),0)) ((p, e) => {
      val (insts0, ptr_13) = getElemPtrInst(aiBase, List(constInt32Zero, getIntConstant(p._2)))
      val insts1 = storeValue(e.dataVal, ptr_13)
      val insts2 = storeMetadata(1, ptr_13, bSize, e.rtMetaData)
      val (insts3, ptr_14) = getElemPtrInst(paramIndices, List(getIntConstant(p._2)))
      val (insts4, ptr_13int) = castInst(PtrtoInt, ptr_13, ptrAsIntType)
      val insts5 = storeValue(ptr_13int, ptr_14)
      (p._1:::cmt("store rtmd of " + e.dataVal, insts0:::insts1:::insts2):::cmt("store the addr into paramIndices", insts3:::insts4:::insts5), p._2 + 1)
    })
    val (instz0) = retType match {
      case Some(t) => {
        val (instz1, retBase, retSize) = dsAlloca(t)
        val (instz2, retBaseStoreAddr) = getElemPtrInst(fmPtr, ApRtMdreturnBufAddr)
        val (instz3, retBaseCharPtr) = castInst(PtrtoInt, retBase, ptrAsIntType)
        val (instz4) = storeValue(retBaseCharPtr, retBaseStoreAddr)
        instz1:::instz2:::instz3:::instz4
      }
      case None => {
        val (instz2, retBaseStoreAddr) = getElemPtrInst(fmPtr, ApRtMdreturnBufAddr)
        val (instz3, retBaseCharPtr) = castInst(InttoPtr, getIntConstant(0), charPtrTy)
        val (instz4) = storeValue(retBaseCharPtr, retBaseStoreAddr)
        instz2:::instz3:::instz4
      }
    }
    (cmt("alloc fmPtr", insty0):::
      cmt("save num of params", insty1:::insty2):::
      cmt("alloc param indices", instx0):::
      cmt("store param indices", insty3:::insty5):::
      cmt("alloc param buf", inst0):::
      cmt("store param buf addr", insty10:::insty11:::insty12):::
      inst1:::
      cmt("store return buf", instz0), fmPtr, ActualParamList(ActualParam(List(), None, fmPtr, List())::list))
  }

  def loadReturnMeta(lhs:Value, fmPtr:VarOrID):List[AbsInst] = {
    val retType = env.typeOf(lhs)
    val edtsTypeCode = getTypeCode(retType)
    val ptrTyp = PointerType(env.typeOf(edtsTypeCode))
    val (inst0, retBaseLocAddr) = getElemPtrInst(fmPtr, ApRtMdreturnBufAddr)
    val (inst1, retBaseAddr) = loadValue(retBaseLocAddr)
    val (inst2, offset) = loadSize(retBaseAddr)
    val (inst3, retBaseAddrInt) = castInst(PtrtoInt, retBaseAddr, ptrAsIntType)
    val (inst4, (m1,m2)) = computeMSECTAddrs(retBaseAddrInt, offset, ptrTyp)
    val st = env.typeOf(edtsTypeCode)
    val (inst5, retTyV) = loadValue(m1, st)
    val (inst6, baseTyV) = loadValue(m2, st)

    registerMetadata(lhs, RtMetaData(retTyV, baseTyV))
    cmt("ld ret md", inst0:::inst1:::inst2:::inst3:::inst4:::inst5:::inst6)
  }

  def processFormalParamList(x:FormalParamList) = {
    val addr = defTab.getNewLocalVar("fmPtr", PointerType(dtsActualParamType), None)
    // load paramNum
    val (instx0, ptr_17) = getElemPtrInst(addr, ApRtMdparamNum)
    val (instx1, paramNum) = loadValue(ptr_17, i32)
    // load paramindices
    val (instx2, ptr_171) = getElemPtrInst(addr, ApRtMdparamAddrIndices)
    val (instx3, paramIndices) = loadValue(ptr_171)
    val (instx4, ptr_170) = getElemPtrInst(addr, ApRtMdparamBufAddr)
    val (instx5, paramBufBase) = loadValue(ptr_170)
    val (instx6, buffSize) = loadSize(paramBufBase)
    val (instx7,_) = x.list.foldLeft((List[AbsInst](), 0)) ((p,n) => {
      val edtsTypeCode = getTypeCode(n.ty)
      val ptrTyp = PointerType(env.typeOf(edtsTypeCode))
      val inst2 = n.id match {
        case Some (oldV) => {
          val (inst3, ptr_19) = getElemPtrInst(paramIndices, List(getIntConstant(p._2)))
          val (inst4, ptr_20) = loadValue(ptr_19)
          val (inst5, rtmd) = loadMetadata(ptr_20, buffSize, getMetaType(n.ty))
          registerMetadata(oldV, rtmd)
          cmt("load rtmd of " + oldV, inst3:::inst4:::inst5)
        }
        case None => List()
      }
      (p._1:::inst2, p._2 + 1)
    })
    val (instx9, callerPPInfo) = loadPPInfo(paramBufBase)
    // initDynamicPPInfo()
    (cmt("load paramNum", instx0:::instx1):::
      cmt("load paramIndices", instx2:::instx3):::
      cmt("load param bufBase", instx4:::instx5):::
      cmt("load param bufBase size", instx6):::
      instx7:::
      cmt("load PPInfo", instx9),
      FormalParamList(FormalParam(PointerType(dtsActualParamType), List(), None, Some(addr), List())::x.list, x.hasDots, x.funAttrs))
  }
}