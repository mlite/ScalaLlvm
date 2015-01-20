package org.scalair.ir.imir

/**
 * User: wangn
 * Date: 4/19/11
 */

object DSTyping {
  val HSECT_SIZE = 2 * 4

  val I8 = "0x01"

  val I16 = "0x8002"
  val I32 = "0x80808003"
  val I64 = "0x8080808080808004"
  val F32 = "0x80808005"
  val F64 = "0x8080808080808006"
  val F96 = "0x8080808080808006"

  val B8 = "0x00"
  val B16 = "0x0000"
  val B32 = "0x00000000"
  val B64 = "0x0000000000000000"

  val GA = "0x80808010"
  val SA = "0x80808020"  // stack address
  val LA = "0x80808030"  // label address
  val FA = "0x80808040"  // function address
  val HA = "0x80808050"  // heap address
  val NEP = "0x7f7f7f00"


  val IXStr:Array[String] = Array("0",
    "1",                "3",                "7",                "F",                // 1-4   bits
    "1F",               "3F",               "7F",               "01",               // 5-8   bits
    "1FF",              "3FF",              "7FF",              "FFF",              // 9-12  bits
    "1FFF",             "3FFF",             "7FFF",             "8002",             // 13-16 bits
    "1FFFF",            "3FFFF",            "7FFFF",            "FFFFF",            // 17-20 bits
    "1FFFFF",           "3FFFFF",           "7FFFFF",           "FFFFFF",           // 21-24 bits
    "1FFFFFF",          "3FFFFFF",          "7FFFFFF",          "FFFFFFF",          // 25-28 bits
    "1FFFFFFF",         "3FFFFFFF",         "7FFFFFFF",         "80808003",         // 29-32 bits
    "1FFFFFFFF",        "3FFFFFFFF",        "7FFFFFFFF",        "FFFFFFFFF",        // 33-36 bits
    "1FFFFFFFFF",       "3FFFFFFFFF",       "7FFFFFFFFF",       "FFFFFFFFFF",       // 33-40 bits
    "1FFFFFFFFFF",      "3FFFFFFFFFF",      "7FFFFFFFFFF",      "FFFFFFFFFFF",      // 41-44 bits
    "1FFFFFFFFFFF",     "3FFFFFFFFFFF",     "7FFFFFFFFFFF",     "FFFFFFFFFFFF",     // 45-48 bits
    "1FFFFFFFFFFFF",    "3FFFFFFFFFFFF",    "7FFFFFFFFFFFF",    "FFFFFFFFFFFFF",    // 49-52 bits
    "1FFFFFFFFFFFFF",   "3FFFFFFFFFFFFF",   "7FFFFFFFFFFFFF",   "FFFFFFFFFFFFFF",   // 53-56 bits
    "1FFFFFFFFFFFFFF",  "3FFFFFFFFFFFFFF",  "7FFFFFFFFFFFFFF",  "FFFFFFFFFFFFFFF",  // 57-60 bits
    "1FFFFFFFFFFFFFFF", "3FFFFFFFFFFFFFFF", "7FFFFFFFFFFFFFFF", "8080808080808004" // 61-64 bits
  );


  val I8Str  =               "01";
  val I16Str =             "8002";
  val I32Str =         "80808003";
  val I64Str = "8080808080808004";

  // floating point types
  val F32Str =         "80808005";
  val F64Str = "8080808080808006"; // 8
  val F80Str = "808080808080808080808007"; // 12

  // uninitialized types
  val B8Str =                "00";
  val B16Str =             "0000";
  val B32Str =         "00000000";
  val B64Str = "0000000000000000";

  val INT_BITS = 32;
  val PTR_BITS = 32;
  val HSECT_LENGTH = 4 * 2;
  val HSECT_SIZE_OFFSET = -4;
  val HSECT_SYMB_OFFSET = -8;
  val GPStr =          "80808010";
  val SPStr =          "80808020";
  val LPStr =          "80808030"; // label pointer
  val FPStr =          "80808040"; // function pointer
  val memcpyFunName = "llvm.memcpy.p0i8.p0i8.i32";
  val memmoveFunName = "llvm.memmove.p0i8.p0i8.i32";
  val memsetFunName = "llvm.memset.p0i8.p0i8.i32";


}

object Constants {
  import DSTyping._

  lazy val i8 = I(8)
  lazy val i16 = I(16)
  lazy val i32 = I(32)
  lazy val i64 = I(64)
  lazy val ptrZero = I(32)

  lazy val int = I(INT_BITS)
  //val ptrAsInt = I(PTR_BITS)

  lazy val zero = IntConst("0")
  private val one = IntConst("1")
  private val two = IntConst("2")
  private val three = IntConst("3")
  private val four = IntConst("4")
  private val five = IntConst("5")
  private val six = IntConst("6")
  private val seven = IntConst("7")


  lazy val constInt8Zero = PrimitiveConst(i8, zero)
  lazy val constInt16Zero = PrimitiveConst(i16, zero)
  lazy val constInt32Zero = PrimitiveConst(i32, zero)
  lazy val constInt64Zero = PrimitiveConst(i64, zero)
  lazy val constPtrZero = PrimitiveConst(I(PTR_BITS), zero)
  lazy val constSizeZero = PrimitiveConst(I(PTR_BITS), zero)

  lazy val constInt96Zero = PrimitiveConst(I(96), zero)

  lazy val const1False = PrimitiveConst(I(1), zero)
  lazy val const1True = PrimitiveConst(I(1), one)

  lazy val constIntZero = PrimitiveConst(int, zero)
  lazy val constIntOne = PrimitiveConst(int, one)
  lazy val constIntTwo = PrimitiveConst(int, two)
  lazy val constIntThree = PrimitiveConst(int, three)
  lazy val constIntFour = PrimitiveConst(int, four)
  lazy val constIntFive = PrimitiveConst(int, five)
  lazy val constIntSix = PrimitiveConst(int, six)
  lazy val constIntSeven = PrimitiveConst(int, seven)

  lazy val constInt32One = PrimitiveConst(int, one)
  lazy val constInt32Two = PrimitiveConst(int, two)
  lazy val constInt32Three = PrimitiveConst(int, three)
  lazy val constInt32Four = PrimitiveConst(int, four)

  lazy val constHSectSizeOffset = PrimitiveConst(i32, IntConst(HSECT_SIZE_OFFSET.toString))
  lazy val constHSectSymbOffset = PrimitiveConst(i32, IntConst(HSECT_SYMB_OFFSET.toString))

  lazy val constIntXZero:Array[PrimitiveConst] =  new Array[PrimitiveConst](66)
  for (i <- 1 to 64) constIntXZero(i) = PrimitiveConst(I(i), zero)

  lazy val dtsTypeCodeIX:Array[PrimitiveConst] = new Array[PrimitiveConst](66)
  for (i <- 1 to 64) dtsTypeCodeIX(i) = PrimitiveConst(I(i), IntConst(IXStr(i)))


  lazy val dtsTypeCodeI8 = PrimitiveConst(i8, IntConst(I8Str))
  lazy val dtsTypeCodeI16 = PrimitiveConst(i16, IntConst(I16Str))
  lazy val dtsTypeCodeI32 = PrimitiveConst(i32, IntConst(I32Str))
  lazy val dtsTypeCodeI64 = PrimitiveConst(i64, IntConst(I64Str))

  lazy val dtsTypeCodeF32 = PrimitiveConst(i32, IntConst(F32Str))
  lazy val dtsTypeCodeF64 = PrimitiveConst(i64, IntConst(F64Str))
  lazy val dtsTypeCodeF80 = PrimitiveConst(I(96), IntConst(F80Str))

  lazy val dtsTypeCodeGP = PrimitiveConst(ptrAsIntType, IntConst(GPStr))
  lazy val dtsTypeCodeFP = PrimitiveConst(ptrAsIntType, IntConst(FPStr))
  lazy val dtsTypeCodeLP = PrimitiveConst(ptrAsIntType, IntConst(LPStr))

  lazy val dtsTypeCodeB8 = PrimitiveConst(i8, IntConst(B8Str))
  lazy val dtsTypeCodeB16 = PrimitiveConst(i16, IntConst(B16Str))
  lazy val dtsTypeCodeB32 = PrimitiveConst(i32, IntConst(B32Str))
  lazy val dtsTypeCodeB64 = PrimitiveConst(i64, IntConst(B64Str))


  lazy val zeroZero = List(constIntZero, constIntZero)
  lazy val zeroOne = List(constIntZero, constIntOne)
  lazy val zeroTwo = List(constIntZero, constIntTwo)
  lazy val zeroThree = List(constIntZero, constIntThree)
  lazy val zeroFour = List(constIntZero, constIntFour)
  lazy val zeroFive = List(constIntZero, constIntFive)
  lazy val zeroSix = List(constIntZero, constIntSix)
  lazy val zeroSeven = List(constIntZero, constIntSeven)


  lazy val charPtrTy = PointerType(i8)
  lazy val charPtrPtrTy = PointerType(charPtrTy)
  lazy val sizePtrType = PointerType(ptrAsIntType)
  lazy val voidType = VoidType ()
  lazy val u8Type = i8
  lazy val u16Type = i16
  lazy val u32Type = i32
  lazy val u64Type = i64
  lazy val umaxBitsType = u64Type

  // for __i386__
  lazy val intType = u32Type
  lazy val ptrAsIntType = u32Type
  lazy val sizeIntType = u32Type

  lazy val oneBitType = I(1)

  lazy val structFileInfoType = StructType(List(charPtrTy, charPtrTy), false)
  lazy val ptrOfStructFileInfoType = PointerType(structFileInfoType)

  lazy val DSRT_struct_fileinfo = structFileInfoType

  lazy val structSrcInfoType = StructType(List(ptrOfStructFileInfoType, intType, intType), false)
  lazy val ptrOfStructSrcInfoType = PointerType(structSrcInfoType)

  lazy val DSRT_struct_srcinfo = structSrcInfoType


  lazy val structPPInfoType = StructType(List(ptrOfStructSrcInfoType, charPtrTy, PointerType(TypeUpRef(2))), false)
  lazy val ptrOfStructPPInfoType = PointerType(structPPInfoType)

  lazy val sizedBuf = StructType (
    List(
      sizeIntType,
      charPtrTy
    ), false)




  def makeFunHeader(retType:Type, id:String, param:List[Type]):VarOrID = {
    lazy val l = param.map(x => FormalParam(x,List(), None, None, List()))
    val fty = FunType(retType, FormalParamList(l, false, List()), List())
    GlobalVar(id)
  }

  lazy val fourPtrAsIntTypes = List(ptrAsIntType, ptrAsIntType, ptrAsIntType, ptrAsIntType)
  lazy val sevenPtrAsIntTypes = fourPtrAsIntTypes:::List(ptrAsIntType, ptrAsIntType, ptrAsIntType)

  lazy val dtsFunCheckBounds =
    makeFunHeader(voidType, "DSRT_check_bound", fourPtrAsIntTypes:::List(ptrOfStructPPInfoType))

  lazy val dtsFunCheckMemset =
    makeFunHeader(voidType, "DSRT_check_memset", fourPtrAsIntTypes:::List(ptrOfStructPPInfoType))


  lazy val dtsFunCheckBoundsExt =
    makeFunHeader(voidType, "DSRT_check_bound_ext", fourPtrAsIntTypes:::List(ptrAsIntType, ptrOfStructPPInfoType))

  lazy val dtsFunCheckMemcpy =
    makeFunHeader(voidType, "DSRT_check_memcpy", sevenPtrAsIntTypes:::List(ptrOfStructPPInfoType))

  lazy val dtsFunCheckMemmove =
    makeFunHeader(voidType, "DSRT_check_memmove", sevenPtrAsIntTypes:::List(ptrOfStructPPInfoType))

  lazy val funCheckCall =
    makeFunHeader(voidType, "DSRT_check_call", fourPtrAsIntTypes:::List(ptrOfStructPPInfoType))

  lazy val llvmMemcpy =
    makeFunHeader(voidType, memcpyFunName, List(charPtrTy, charPtrTy, sizeIntType, sizeIntType, oneBitType))

  lazy val llvmMemmove =
    makeFunHeader(voidType, memmoveFunName, List(charPtrTy, charPtrTy, sizeIntType, sizeIntType, oneBitType))

  lazy val llvmMemset =
    makeFunHeader(voidType, memsetFunName, List(charPtrTy, u8Type, sizeIntType, sizeIntType, oneBitType))

  lazy val llvmMalloc =
    makeFunHeader(charPtrTy, "malloc", List(sizeIntType))

  lazy val dsrtMalloc =
    makeFunHeader(voidType, "DSRT_malloc", List(PointerType(VoidType()), sizeIntType, charPtrPtrTy,
      PointerType(u32Type), PointerType(u32Type)))

  lazy val funLogPPAddr =
    makeFunHeader(voidType, "DSRT_log_pp_addr", List(intType, ptrOfStructPPInfoType, charPtrTy, ptrAsIntType))

  lazy val funLogPPMsg =
    makeFunHeader(voidType, "DSRT_log_pp_msg", List(intType, ptrOfStructPPInfoType, charPtrTy))

  lazy val funLogPPVal =
    makeFunHeader(voidType, "DSRT_log_pp_val", List(intType, ptrOfStructPPInfoType, charPtrTy, sizeIntType))


  lazy val funLogSrcInfo =
    makeFunHeader(voidType, "DSRT_print_srcinfo", List(intType, ptrOfStructSrcInfoType))

  lazy val funLogPPSymAndMsg =
    makeFunHeader(voidType, "DSRT_log_pp_sym_and_msg", List(intType, ptrOfStructPPInfoType, charPtrTy))

  lazy val funPrintDynamicPPInfo =
    makeFunHeader(voidType, "DSRT_log_print_dynamic_ppinfo", List(ptrOfStructPPInfoType, charPtrTy))


  lazy val funLogAddr =
    makeFunHeader(voidType, "DSRT_log_addr", List(intType, charPtrTy, ptrAsIntType))

  lazy val funLogMsg =
    makeFunHeader(voidType, "DSRT_log_msg", List(intType, charPtrTy))

  lazy val funLogVal =
    makeFunHeader(voidType, "DSRT_log_val", List(intType, charPtrTy, sizeIntType))


  lazy val dtsFunAddTypeCode =
    makeFunHeader(ptrAsIntType, "DSRT_p_add", List(ptrAsIntType, ptrAsIntType))

  lazy val dtsFunSubTypeCode =
    makeFunHeader(ptrAsIntType, "DSRT_p_sub", List(ptrAsIntType, ptrAsIntType))

  lazy val pushStackPType =
    makeFunHeader(ptrAsIntType, "DSRT_push_stack_ptype", List())

  lazy val popStackPType =
    makeFunHeader(ptrAsIntType, "DSRT_pop_stack_ptype", List())

  lazy val funZeroMSECT =
    makeFunHeader(voidType, "DSRT_zero_msect", List(charPtrTy, sizeIntType))
}