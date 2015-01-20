package org.scalair.ir.llvm

import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.input.Reader
import org.scalair.ir.common._

/**
 * User: wangn
 * Date: Mar 8, 2011
 */

// we have to keep states such as declared NamedTypes,
// so we are class. We need a new instance for each module
class LLVMParser() extends StandardTokenParsers  {
  override val lexical = LLVMLexer;
  import lexical.{_};

  var namedTypes:Set[String] = Set()
  var unnamedTypes:Set[String] = Set()
  var unnamedTypeCount = 0;
  var blockSerno:Int = 0;
  var currentFun:GlobalVarOrID = GlobalVar("")
  var funBlockMap:Map[GlobalVarOrID, Map[Int, Block]] = Map()

  lexical.delimiters ++= List("=", "[", "]", "{", "}", "<", ">", "(", ")", ",", "*", "\\", "!")
  var keywords = List(
    "begin", "end", "true", "false", "declare", "define", "global", "constant", "private", "linker_private",
    "linker_private_weak", "linker_private_weak_def_auto", "internal", "available_externally", "linkonce",
    "linkonce_odr", "weak", "weak_odr", "appending", "dllimport", "dllexport", "common", "default", "hidden",
    "protected", "extern_weak", "external", "thread_local", "zeroinitializer", "undef", "null", "to", "tail",
    "target", "triple", "deplibs", "datalayout", "volatile", "nuw", "nsw",
    "exact", "inbounds", "align", "addrspace", "section", "alias", "module", "asm", "sideeffect",
    "alignstack", "gc", "ccc", "fastcc", "coldcc", "x86_stdcallcc", "x86_fastcallcc", "x86_thiscallcc", "arm_apcscc",
    "arm_aapcscc", "arm_aapcs_vfpcc", "msp430_intrcc", "cc", "c", "signext", "zeroext", "inreg", "sret",
    "nounwind", "noreturn", "noalias", "nocapture", "byval", "nest", "readnone", "readonly", "inlinehint",
    "noinline", "alwaysinline", "optsize", "ssp", "sspreq", "noredzone", "noimplicitfloat", "naked", "type", "opaque",
    "eq", "ne", "slt", "sgt", "sle", "sge", "ult", "ugt", "ule", "uge", "oeq", "one", "olt", "ogt", "ole",
    "oge", "ord", "uno", "ueq", "une", "x", "blockaddress"
  )

  val instkeywords = List(
    "add",  "fadd", "sub",  "fsub", "mul",  "fmul", "udiv", "sdiv", "fdiv", "urem", "srem", "frem",
    "shl",  "lshr", "ashr", "and",  "or",   "xor", "icmp", "fcmp", "phi", "call", "trunc",
    "zext", "sext", "fptrunc", "fpext", "uitofp", "sitofp", "fptoui", "fptosi", "inttoptr", "ptrtoint",
    "bitcast", "select", "va_arg", "ret", "br", "switch", "indirectbr", "invoke", "unwind", "unreachable",
    "alloca", "malloc", "load", "store", "getelementptr", "extractelement", "insertelement", "shufflevector",
    "getresult", "extractvalue", "insertvalue", "free"
  )

  lexical.reserved ++=keywords
  lexical.reserved ++=instkeywords


  def localVar: Parser[LocalVar] = elem("LOCAL_VAR", _.isInstanceOf[LocalVarTok]) ^^ { case x => LocalVar(x.chars) }
  def localID:Parser[LocalID] = elem("LOCAL_ID", _.isInstanceOf[LocalIDTok]) ^^ { case x => LocalID(x.chars) }
  def localVarOrID = (localVar | localID)

  def globalVar: Parser[GlobalVar] = elem("GLOBAL_VAR", _.isInstanceOf[GlobalVarTok]) ^^ { case x => GlobalVar(x.chars) }
  def globalID: Parser[GlobalID] = elem("GLOBAL_ID", _.isInstanceOf[GlobalIDTok]) ^^ { case x => GlobalID(x.chars) }
  def globalVarOrID = (globalVar | globalID)

  def astLabelDef: Parser[Label] =
    (elem("LABEL", _.isInstanceOf[LabelStrTok]) ^^ { case x => Label(x.chars) })

  def mdVar: Parser[MDVar] = elem("MD_VAR", _.isInstanceOf[MDVarTok]) ^^ { case x => MDVar(x.chars) }

  override def numericLit:Parser[String] =
    elem("INT_CONST", (x => x.isInstanceOf[PIntTok] || x.isInstanceOf[NIntTok] ||
      x.isInstanceOf[HexIntTok])) ^^ (_.chars)

  def quoteStr:Parser[QuoteStr] = super.stringLit ^^ { s => QuoteStr(s) }


  def floatConst:Parser[FloatConst] =
    elem("FLOAT_CONST", (x => x.isInstanceOf[FPTok] || x.isInstanceOf[HexFPTok] ||
      x.isInstanceOf[HexFP80Tok] || x.isInstanceOf[HexFP128Tok] || x.isInstanceOf[HexPPC128Tok])) ^^
      { case x => FloatConst(x.chars) }

  def dotDotDot: Parser[String] =
    elem("DOTDOTDOT", _.isInstanceOf[DotDotDotTok]) ^^ (_.chars)

  def astVoidType: Parser[Type] =
    elem("VOID", _.isInstanceOf[VoidTok]) ^^ { case _ => VoidType() }

  def astAtomicType: Parser[Type] =
    (elem("TYPE", _.isInstanceOf[TypeTok]) ^^ {
      case TypeTok("i1") => I(1)
      case TypeTok("i8") => I(8)
      case TypeTok("i16") => I(16)
      case TypeTok("i32") => I(32)
      case TypeTok("i64") => I(64)
      case TypeTok("f32") => F(32)
      case TypeTok("f64") => F(64)
      case TypeTok("f80") => F(80)
      case TypeTok("f128") => F(128)
      case TypeTok("v64") => V64()
      case TypeTok("v128") => V128()
      case TypeTok("float") => FloatType()
      case TypeTok("double") => DoubleType()
      case TypeTok("void") => VoidType()
      case TypeTok("label") => LabelType()
      case TypeTok("opaque") => OpaqueType()
      case TypeTok("metadata") => MetadataType()
      case TypeTok(str) => {
        if (str.startsWith("i"))
          I(Integer.parseInt(str.substring(1,str.length)))
        else
          TypeName(str)
      }}
      | localVar ^^ { v => namedTypes += v.toString; TypeName(v.toString) }
      | quoteStr ^^ { str => namedTypes += str.toString; TypeName(str.toString) }
      | localID ^^ { case LocalID(s) => unnamedTypes += s.toString; TypeNo(Integer.parseInt(s)) }
      | astVoidType)

  def astTypeUpref:Parser[TypeUpRef] =
    "\\" ~> numericLit ^^ { s => TypeUpRef(Integer.parseInt(s)) }

  def astArrayType:Parser[ArrayType] =
    "[" ~ numericLit ~ "x" ~ astType ~ "]" ^^ { case "[" ~ n ~ "x" ~ t ~ "]" => ArrayType(Integer.parseInt(n), t) }

  def astVectorType:Parser[VectorType] =
    "<" ~ numericLit ~ "x" ~ astType ~ ">" ^^ { case "<" ~ n ~ "x" ~ t ~ ">" => VectorType(Integer.parseInt(n), t) }

  def astStructType:Parser[StructType] =
    ("{" ~ repsep (astType, ",") ~ "}" ^^ { case "{" ~ l ~ "}" => StructType (l, false) }
      | "<" ~ "{" ~ repsep (astType, ",") ~ "}" ~ ">" ^^ { case "<" ~ "{" ~ l ~ "}" ~ ">" => StructType (l, true) })


  def astProtoArgType:Parser[Type] =
    astType ~ rep(astAttr) ~ opt(localVar) ^^ { case t ~ att ~ v => t  }

  // the straigtforward defintion of astType has lhs recursions
  // in pointer type -- astType * and function type -- astType (...)
  // we have to use lhs refactoring to remove these lhs recursions.
  def astType:Parser[Type] = {
    def astSureType:Parser[Type] = (astAtomicType | astTypeUpref | astArrayType | astVectorType | astStructType)

    def arguments =
      astFormalParamList ~ rep(astFunAttr) ^^ {
        case p ~ a => (p, a)
      }

    def astStar = opt(astAddrSpace) ~ "*" ^^ { case addr ~ "*" => addr }

    (astSureType ~ rep(astStar|arguments) ^^ {
      case b ~ stars => stars.foldLeft(b) ((b0, a) => a match {
        case addrspace:Option[AddrSpace] => PointerType(b0, addrspace)
        case args:(FormalParamList, List[Attr]) => FunType(b0, args._1, args._2)
      })
    })
  }

  def astConst:Parser[Const] =
    ( numericLit ^^ { case i => IntConst(i) }
      | "c" ~ quoteStr ^^ { case "c" ~ QuoteStr(s) => StrConst(s) }
      | "null" ^^ { case _ => NullValue() }
      | "undef" ^^ { case _ => Undef() }
      | "false" ^^ { case _ => BFalse() }
      | "true" ^^ { case _ => BTrue() }
      | "zeroinitializer" ^^ { case _ => Zero() }
      | quoteStr ^^ { s => LabelQuoteStr(s) }
      | globalVarOrID ^^ { s => GlobalAddr(s) }
      | floatConst
      | astPackedStructVal
      | astStructVal
      | astVectorVal
      | astArrayVal
      | astConstExpr
      | astBlockAddr
      | astMetaConst
      )

  def astConstCast:Parser[ConstCast] =
    astConvOp ~ "(" ~ astTC ~ "to" ~ astType ~ ")" ^^
      { case conv ~ "(" ~ tv ~ "to" ~ t ~ ")" => ConstCast(conv, tv, t) }

  def astConstGetElemPtr:Parser[ConstGetElemPtr] =
    "getelementptr" ~ opt("inbounds") ~ "(" ~ astTC ~ "," ~ rep1sep (astTC, ",") ~ ")" ^^
      { case "getelementptr" ~ inb~  "(" ~ base ~ "," ~ indices ~ ")" => ConstGetElemPtr(inb.isDefined, base, indices) }

  def astConstArithmaticExpr:Parser[ConstArithmaticExpr] =
    astArithmaticOp ~ rep(astCarry) ~ "(" ~ astTC ~ "," ~ astTC ~ ")" ^^
      { case b ~ carry ~ "(" ~ tv ~ "," ~ o1 ~ ")" => ConstArithmaticExpr(b, carry, tv, o1) }

  def astConstBitwiseExpr:Parser[ConstBitwiseExpr] =
    astBitwiseOp ~ "(" ~ astTC ~ "," ~ astTC ~ ")" ^^
      { case b ~ "(" ~ tv ~ "," ~ o1 ~ ")" => ConstBitwiseExpr(b, tv, o1) }

  def astConstICmpExpr:Parser[ConstICmpExpr] =
    "icmp" ~ astICmpOp ~ "(" ~ astTC ~ "," ~ astTC ~ ")" ^^
      { case "icmp" ~ op ~ "(" ~ tv ~ "," ~ v ~ ")" => ConstICmpExpr(op, tv, v) }

  def astConstFCmpExpr:Parser[ConstFCmpExpr] =
    "fcmp" ~ astFCmpOp ~ "(" ~ astTC ~ "," ~ astTC ~ ")" ^^
      { case "fcmp" ~ op ~ "(" ~ tv ~ "," ~ v ~ ")" => ConstFCmpExpr(op, tv, v) }

  def astOtherConst:Parser[Const] =
    ("extractelement" ~ "(" ~ astTC ~ "," ~ astTC ~ ")" ^^
      { case "extractelement" ~ "(" ~ vect ~ "," ~ index ~ ")" => ConstExtractElement(vect, index) }
      |"insertelement" ~ "(" ~ astTC ~ "," ~ astTC ~ "," ~ astTC ~ ")" ^^
      { case "insertelement" ~ "(" ~ vect ~ "," ~ v ~"," ~ index ~ ")" => ConstInsertElement(vect, v, index)}
      |"shufflevector" ~ "(" ~ astTC ~ "," ~ astTC ~ "," ~ astTC ~ ")" ^^
      { case "shufflevector" ~ "(" ~ vect ~ "," ~ v ~"," ~ index ~ ")" => ConstShuffleVector(vect, v, index)}
      |"extractvalue" ~ "(" ~ astTC ~ "," ~ rep1sep (numericLit, ",") ~ ")" ^^
      { case "extractvalue" ~ "(" ~ tv ~ "," ~ indices ~ ")" => ConstExtractValue(tv, indices) }
      |"insertvalue" ~ "(" ~ astTC ~ "," ~ astTC ~ "," ~ rep1sep(numericLit, ",") ~ ")" ^^
      { case "insertvalue" ~ "(" ~ tv ~ "," ~ e ~ "," ~ indices ~ ")" => ConstInsertValue(tv, e, indices) }
      |"select" ~ "(" ~ astTC ~ "," ~ astTC ~ "," ~ astTC ~ ")" ^^
      { case "select" ~ "(" ~ cond ~ "," ~ tv1 ~ "," ~ tv2 ~ ")" => ConstSelect(cond, tv1, tv2) }
      )

  def astConstExpr:Parser[Const] =
    astConstCast | astConstGetElemPtr | astConstArithmaticExpr | astConstBitwiseExpr |
      astConstICmpExpr | astConstFCmpExpr | astOtherConst

  def astMDNode:Parser[MDNode] =
    "!" ~ numericLit ^^ { case "!" ~ n => MDNode(n) }

  def astMetaConst:Parser[MetaConst] =
    ("!" ~ astStructVal ^^ { case "!" ~ v => MDConst(v) }
      | "!" ~ quoteStr ^^ { case "!" ~ s => MDString(s) }
      | astMDNode
      | localVarOrID ^^ { s => MDRef(s) }
      )

  def astStructVal:Parser[StructConst] =
    "{" ~ repsep(astTC,",") ~ "}" ^^ { case "{" ~ l ~ "}" => StructConst(l, false) }

  def astPackedStructVal:Parser[StructConst] =
    "<"~"{" ~ repsep(astTC,",") ~ "}" ~ ">" ^^ { case "<"~"{" ~ l ~ "}"~">" => StructConst(l, true) }

  def astVectorVal:Parser[VectorConst] =
    "<" ~  rep1sep (astTC, ",") ~ ">" ^^ { case "<" ~ l ~ ">" => VectorConst(l)}

  def astArrayVal:Parser[ArrayConst] =
    "[" ~  repsep (astTC, ",") ~ "]" ^^ { case "[" ~ l ~ "]" => ArrayConst(l)}

  def astBlockAddr:Parser[BlockAddress] =
    "blockaddress" ~ "(" ~ globalVar ~ "," ~ localVar ~ ")" ^^
      { case "blockaddress" ~ "(" ~ gv ~ "," ~ lv ~ ")" => BlockAddress(gv, lv) }

  def astVarOrID = (globalVarOrID | localVarOrID)

  def astValue:Parser[Value] = (astVarOrID  | astConst | astInlineAsm )

  def astExternLinkage:Parser[Linkage] =
     ("external" ^^ { case _ => External }
       | "extern_weak" ^^ { case _ => Extern_weak }
       | "dllimport" ^^ { case _ => Dllimport })

  def astLinkage:Parser[Linkage] =
    ("private" ^^ { _ => Private}
      |"linker_private" ^^ { case _ => Linker_private }
      |"linker_private_weak" ^^ { case _ => Linker_private_weak }
      |"linker_private_weak_def_auto" ^^ { case _ => Linker_private_weak_def_auto }
      |"internal" ^^ { case _ => Internal }
      |"available_externally" ^^ { case _ => Available_externally }
      |"linkonce" ^^ { case _ => Linkonce }
      |"weak" ^^ { case _ => Weak }
      |"common" ^^ { case _ => Common }
      |"appending" ^^ { case _ => Appending }
      |"linkonce_odr" ^^ { case _ => Linkonce_odr }
      |"weak_odr" ^^ { case _ => Weak_odr }
      |"dllexport" ^^ { case _ => Dllexport })

  def astAliasLinkage:Parser[Linkage] =
    ("external" ^^ { case _ => External}
      | "internal" ^^ { case _ => Internal }
      | "weak" ^^ { case _ => Weak }
      | "weak_odr" ^^ { case _ => Weak_odr})

  def astAllLinkage:Parser[Linkage] =
    astExternLinkage | astLinkage

  def astCConv:Parser[CConv] =
      ("ccc" ^^ { case _ => CCC }
        |"fastcc" ^^ { case _ => Fast }
        |"coldcc" ^^ { case _ => Cold }
        |"x86_stdcallcc" ^^ { case _ => X86_stdcall }
        |"x86_fastcallcc" ^^ { case _ => X86_fastcall }
        |"x86_thiscallcc" ^^ { case _ => X86_thiscall }
        |"arm_apcscc" ^^ { case _ => Arm_apcs }
        |"arm_aapcscc" ^^ { case _ => Arm_aapcs }
        |"arm_aapcs_vfpcc" ^^ { case _ => Arm_aapcs_vfp }
        |"msp430_intrcc" ^^ { case _ => Msp430_intr }
        |elem("CCONV_KEYWORD", _.isInstanceOf[CConvTok]) ^^ { case x => CC(x.chars) })

  def astAttr:Parser[Attr] =
    ("zeroext" ^^ { case _ => ZeroExt }
      |"signext" ^^ { case _ => SignExt }
      |"inreg" ^^ { case _ => InReg }
      |"byval" ^^ { case _ => ByVal }
      |"sret" ^^ { case _ => SRet }
      |"noalias" ^^ { case _ => NoAlias }
      |"nocapture" ^^ { case _ => NoCapture }
      |"nest" ^^ { case _ => Nest }
      | astFunAttr)

  def astAlignStack:Parser[AlignStack] =
    "alignstack" ~ "(" ~ numericLit ~ ")" ^^
      { case "alignstack" ~ "(" ~ n ~ ")" => AlignStack(Integer.parseInt(n)) }

  def astFunAttr:Parser[FunAttr] =
    (astAlignStack
      |"alwaysinline" ^^ { case _ => AlwaysInline }
      |"hotpatch" ^^ { case _ => HotPatch }
      |"inlinehint" ^^ { case _ => InlineHint }
      |"naked" ^^ { case _ => Naked }
      |"noimplicitfloat" ^^ { case _ => NoImplicitFloat }
      |"noinline" ^^ { case _ => NoInline }
      |"noredzone" ^^ { case _ => NoRedzone }
      |"noreturn" ^^ { case _ => NoReturn }
      |"nounwind" ^^ { case _ => NoUnwind }
      |"optsize" ^^ { case _ => OptSize }
      |"readnone" ^^ { case _ => ReadNone }
      |"readonly" ^^ { case _ => ReadOnly }
      |"ssp" ^^ { case _ => Ssp }
      |"sspreq" ^^ { case _ => SspReq })

  def astArithmaticOp:Parser[ArithmaticOp] =
    ("add" ^^ { case _ => Add }
      |"fadd" ^^ { case _ => FAdd }
      |"sub" ^^ { case _ => Sub }
      |"fsub" ^^ { case _ => FSub }
      |"mul" ^^ { case _ => Mul }
      |"fmul" ^^ { case _ => FMul }
      |"udiv" ^^ { case _ => UDiv }
      |"sdiv" ^^ { case _ => SDiv }
      |"fdiv" ^^ { case _ => FDiv }
      |"urem" ^^ { case _ => URem }
      |"srem" ^^ { case _ => SRem }
      |"frem" ^^ { case _ => FRem })

  def astCarry:Parser[Carry] =
    ("nuw" ^^ { _ => Nuw }
      | "nsw" ^^ { _ => Nsw }
      | "exact" ^^ { _ => Exact }
      )

  def astBitwiseOp:Parser[BitwiseOp] =
    ("shl" ^^ { case _ => Shl }
      |"lshr" ^^ { case _ => LShr }
      |"ashr" ^^ { case _ => AShr }
      |"and" ^^ { case _ => And }
      |"or" ^^ { case _ => Or }
      |"xor" ^^ { case _ => Xor})

  def astConvOp:Parser[ConvOp] =
    ("trunc" ^^ { case _ => Trunc }
      |"zext" ^^ { case _ => ZExt }
      |"sext" ^^ { case _ => SExt }
      |"fptrunc" ^^ { case _ => FPTrunc }
      |"fpext" ^^ { case _ => FPExt }
      |"fptoui" ^^ { case _ => FPtoUI }
      |"fptosi" ^^ { case _ => FPtoSI }
      |"uitofp" ^^ { case _ => UItoFP }
      |"sitofp" ^^ { case _ => SItoFP }
      |"ptrtoint" ^^ { case _ => PtrtoInt }
      |"inttoptr" ^^ { case _ => InttoPtr}
      |"bitcast" ^^ { case _ => Bitcast})

  def astICmpOp:Parser[ICmpOp] =
    ("eq" ^^ { case _ => ICmpEq }
      | "ne" ^^ { case _ => ICmpNe }
      | "slt" ^^{ case _ => ICmpSlt }
      | "sgt" ^^ { case _ => ICmpSgt }
      | "sle" ^^ { case _ => ICmpSle }
      | "sge" ^^ { case _ => ICmpSge }
      | "ult" ^^ { case _ => ICmpUlt }
      | "ugt" ^^ { case _ => ICmpUgt }
      | "ule" ^^ { case _ => ICmpUle }
      | "uge" ^^ { case _ => ICmpUge })

  def astFCmpOp:Parser[FCmpOp] =
    ("oeq" ^^ { case _ => FCmpOeq }
      | "one" ^^ { case _ => FCmpOne }
      | "olt" ^^{ case _ => FCmpOlt }
      | "ogt" ^^ { case _ => FCmpOgt }
      | "ole" ^^ { case _ => FCmpOle }
      | "oge" ^^ { case _ => FCmpOge }
      | "ord" ^^ { case _ => FCmpOrd }
      | "uno" ^^ { case _ => FCmpUno }
      | "ueq" ^^ { case _ => FCmpUeq }
      | "une" ^^ { case _ => FCmpUne }
      | "ult" ^^ { case _ => FCmpUlt }
      | "ugt" ^^ { case _ => FCmpUgt }
      | "ule" ^^ { case _ => FCmpUle }
      | "uge" ^^ { case _ => FCmpUge }
      | "true" ^^ { case _ => FCmpTrue }
      | "false" ^^ { case _ => FCmpFalse })

  def astVisibility:Parser[Visibility] =
    ("default" ^^ { case _ => Default }
      |"hidden" ^^ { case _ => Hidden }
      |"protected" ^^ { case _ => Protected })

  def astTC:Parser[TC] =
    ("null" ^^ { case "null" => NullTV() }
      |astType ~ astConst ^^ { case t ~ v => new TC(t, v) })

  def astTV:Parser[TV] =
    ("null" ^^ { case "null" => NullTV() }
      |astType ~ astValue ^^ { case t ~ v => new TV(t, v) })

  def astAlign:Parser[Align] = "align" ~ numericLit ^^ { case "align" ~ s => Align(Integer.parseInt(s)) }
  def astSection:Parser[Section] = "section" ~ quoteStr ^^ { case "section" ~ s => Section(s) }

  def astMemOp:Parser[MemOp] = {
    def astAlloca:Parser[Alloca] = "alloca" ~ astType ~ opt("," ~> astTV) ~ opt("," ~> astAlign) ^^
      { case "alloca" ~ t ~ elem ~ align =>  Alloca(t, elem, align) }

    def astMalloc:Parser[Malloc] = "malloc" ~ astType ~ opt("," ~> astTV) ~ opt("," ~> astAlign) ^^
      { case "malloc" ~ t ~ size ~ align => Malloc(t, size, align) }

    def astFree:Parser[Free] = "free" ~ astTV ^^ { case "free" ~ tv => Free(tv) }

    def astLoad =
      opt("volatile") ~ "load" ~ astTV ~ opt("," ~> astAlign) ^^
        { case vol ~ "load" ~ tv ~ align => Load (vol.isDefined, tv, align) }

    def astStore =
      opt("volatile") ~ "store" ~ astTV ~ "," ~ astTV ~ opt("," ~> astAlign) ^^
        { case vol ~ "store" ~ v ~ "," ~ ptr ~ align => Store (vol.isDefined, v, ptr, align) }

    (astAlloca | astMalloc | astFree | astLoad | astStore)
  }

  def astGetElementPtr =
    "getelementptr" ~ opt("inbounds") ~ astTV ~ "," ~ rep1sep (astTC, ",") ^^
      { case "getelementptr" ~ ibopt ~ base ~ "," ~ l => GetElemPtr(base, l) }

  def astGetResult =
    "getresult" ~  astTV ~ "," ~ numericLit ^^
      { case "getresult" ~ tv ~ "," ~ i => GetResult(tv, i) }

  def astExpr:Parser[Expr] =
    (astGetElementPtr | astGetResult | astArithmaticExpr | astBitwiseExpr | astCast | astICmpExpr | astFCmpExpr)

  def astArithmaticExpr:Parser[ArithmaticExpr] =
    astArithmaticOp ~ rep(astCarry) ~ astType ~ astValue ~ "," ~ astValue ^^
      { case b ~ carry ~ t ~ v1 ~ "," ~ v2 => ArithmaticExpr(b, carry, t, v1, v2) }

  def astBitwiseExpr:Parser[BitwiseExpr] =
    astBitwiseOp ~ astType ~ astValue ~ "," ~ astValue ^^ {
      case b ~ t ~ v1 ~ "," ~ v2 => BitwiseExpr(b, t, v1, v2)
    }

  def astICmpExpr:Parser[ICmpExpr] =
    "icmp" ~ astICmpOp ~ astType ~ astValue ~ "," ~ astValue ^^ {
      case "icmp" ~ op ~ t~ v1 ~ "," ~ v2 => ICmpExpr(op, t, v1, v2)
    }

  def astFCmpExpr:Parser[FCmpExpr] =
    "fcmp" ~ astFCmpOp ~ astType ~ astValue ~ "," ~ astValue ^^ {
      case "fcmp" ~ op ~ t ~ v1 ~ "," ~ v2 => FCmpExpr(op, t, v1, v2)
    }

  def astCast:Parser[CastExpr] =
    astConvOp ~ astTV ~ "to" ~ astType ^^ { case op ~ tv ~ "to" ~ t =>  CastExpr(op, tv, t) }

  def astCall:Parser[Call] =
    opt("tail") ~ "call" ~ opt(astCConv) ~ rep(astAttr) ~ astType ~ astVarOrID ~ astActualParamList ~ rep(astAttr) ^^ {
      case tail ~ "call" ~ cc ~ retAttrs ~ retType ~ fn ~ params ~ funAttrs => Call(cc, retAttrs, retType, fn, params, funAttrs)
    }

  def astVaArg:Parser[VaArg] =
    "va_arg" ~ astTV ~ "," ~ astType ^^ { case "va_arg" ~ tv ~ "," ~ t => VaArg(tv, t) }


  def astPhi:Parser[Phi] = {
    def valPairs = "[" ~ astValue ~ "," ~ astValue ~ "]" ^^ { case "[" ~ v0 ~ "," ~ v1 ~ "]" => (v0, v1) }
    "phi" ~ astType ~ rep1sep (valPairs, ",") ^^ { case "phi" ~ t ~ l => Phi(t, l) }
  }

  def astInlineAsm:Parser[InlineAsm] =
    "asm" ~ opt("sideeffect") ~ opt("alignstack") ~ quoteStr ~ "," ~ quoteStr ^^
      { case "asm" ~ sideeffect ~ alignStack ~ s1 ~ "," ~ s2 =>
        InlineAsm (sideeffect.isDefined, alignStack.isDefined, s1, s2)
      }

  def astOther:Parser[RHS] = {
    ("extractelement" ~ astTV ~ "," ~ astTV ^^
      { case "extractelement" ~ vect ~ "," ~ index => ExtractElement(vect, index)}
      |"insertelement" ~ astTV ~ "," ~ astTV ~ "," ~ astTV ^^
      { case "insertelement" ~ vect ~ "," ~ v ~"," ~ index => InsertElement(vect, v, index)}
      |"shufflevector" ~ astTV ~ "," ~ astTV ~ "," ~ astTV ^^
      { case "shufflevector" ~ vect ~ "," ~ v ~"," ~ index => ShuffleVector(vect, v, index)}
      |"extractvalue" ~ astTV ~ "," ~ rep1sep (numericLit, ",") ^^
      { case "extractvalue" ~ tv ~ "," ~ indices => ExtractValue(tv, indices) }
      |"insertvalue" ~ astTV ~ "," ~ astTV ~ "," ~ rep1sep(numericLit, ",") ^^
      { case "insertvalue" ~ tv ~ "," ~ e ~ "," ~ indices => InsertValue(tv, e, indices) }
      |"select" ~ astTV ~ "," ~ astTV ~ "," ~ astTV ^^
      { case "select" ~ cond ~ "," ~ tv1 ~ "," ~ tv2 => Select(cond, tv1, tv2) }
      )
  }

  def astRHS:Parser[RHS] =
    (astMemOp | astExpr | astCall | astPhi | astOther | astVaArg)

  def astInstructionMD = {
    def dbg = mdVar ~ astMetaConst ^^ { case MDVar(s) ~ mv => Dbg(s,  mv) }
    rep("," ~> dbg)
  }

  def astInstruction:Parser[Instruction] =
    ( astVarOrID ~ "=" ~ astRHS ~ astInstructionMD ^^ { case id ~ "=" ~ rhs ~ dbg  => Instruction (Some(id), rhs, dbg) }
      | quoteStr ~ "=" ~ astRHS ~ astInstructionMD ^^ { case id ~ "=" ~ rhs ~ dbg => Instruction (Some(LocalStr(id)), rhs, dbg) }
      | astRHS ~ astInstructionMD ^^ { case rhs ~ dbg => Instruction(None, rhs, dbg) }
      )

  def astControlInstWithDbg =
    astControlInst ~ astInstructionMD ^^ { case inst ~ dbg => ControlInstDbg(inst, dbg) }


  def astInvoke:Parser[Invoke] =
    opt(astVarOrID <~ "=") ~ "invoke" ~ opt(astCConv) ~ rep(astAttr) ~ astTV ~ astActualParamList ~ rep(astAttr) ~
      "to" ~ astTV ~ "unwind" ~ astTV ^^
      { case lhs ~ "invoke" ~ cc ~ retAttr ~ fn ~ params ~ fnAttrs ~ "to" ~ l ~ "unwind" ~ ex =>
        Invoke(lhs, cc, retAttr, fn, params, fnAttrs, l, ex)
      }

  def astCase:Parser[(TV, TV)] = astTV ~ "," ~ astTV ^^ { case tv ~ "," ~ lb => (tv, lb) }

  def astIndirectBr:Parser[IndirectBr] =
    "indirectbr" ~ astTV ~ "," ~ "[" ~ repsep (astTV, ",") ~ "]" ^^
      { case "indirectbr" ~ v ~ "," ~ "[" ~ l ~ "]" => IndirectBr(v, l) }

  def astRetValue:Parser[Ret] = {
    def parseValue(s:Type) = s match {
      case VoidType() => success(None) ^^ { _ => Ret(List()) }
      case t => astValue ~ rep("," ~> astTV) ^^ { case v ~ l => Ret(new TV(t,v)::l) }
    }
    "ret" ~> astType into parseValue
  }

  def astControlInst:Parser[ControlInst] =
    ( "unreachable" ^^ { case "unreachable" => Unreachable() }
      | "br" ~ astTV ~ "," ~ astTV ~ "," ~ astTV ^^ { case "br" ~ c ~ "," ~ t ~ "," ~ f => CBr(c, t,f) }
      | "br" ~ astTV ^^ { case "br" ~ d => Br(d) }
      | "switch" ~ astTV ~ "," ~ astTV ~ "[" ~ rep(astCase) ~ "]" ^^
      { case "switch" ~ tv ~ "," ~ default ~ "[" ~ cases ~ "]" => Switch(tv, default, cases) }
      | "unwind" ^^ { case _ => Unwind() }
      | astInvoke
      | astRetValue
      | astIndirectBr)

  def astFormalParamList:Parser[FormalParamList] = {
    def astFormalParam:Parser[FormalParam] =
      astType ~ rep(astAttr) ~ opt(astAlign) ~ opt(localVar) ~ rep(astAttr) ^^
        { case tv ~ attrs1 ~ align ~ lv ~ attrs2 => FormalParam(tv, attrs1, align, lv, attrs2) }

    ("(" ~ opt(dotDotDot) ~ ")" ~ rep(astAttr) ^^
      { case "(" ~ dots ~ ")" ~ fnAttrs => FormalParamList(List(), dots.isDefined, fnAttrs) }

      |"(" ~ rep1sep(astFormalParam, ",") ~ opt("," ~> dotDotDot) ~ ")" ~ rep(astAttr) ^^
      { case "(" ~ l ~ dots  ~ ")" ~ fnAttrs => FormalParamList(l, dots.isDefined, fnAttrs)
      })
  }

  def astActualParamList:Parser[ActualParamList] = {
    def astActualParam:Parser[ActualParam] =
      astType ~ rep(astAttr) ~ opt(astAlign) ~ astValue ~ rep(astAttr) ^^
        { case tv ~ attrs1 ~ align ~ lv ~ attrs2 => ActualParam(tv, attrs1, align, lv, attrs2) }

    "(" ~ repsep(astActualParam, ",") ~ ")" ^^ { case "(" ~ l ~ ")" => ActualParamList(l) }
  }

  def astFunctionHeader:Parser[FunctionHeader] = {
    def protoHead =
      opt(astAllLinkage) ~ opt(astVisibility) ~ opt(astCConv) ~ rep(astAttr) ~ astType ~ globalVarOrID ^^
        { case lopt ~ vopt ~ copt ~ attrs ~ ret ~ name => {
          currentFun = name; blockSerno = 0; (lopt, vopt, copt, attrs, ret, name) }
        }

    def astGC:Parser[GC] = "gc" ~> quoteStr ^^ { case s => GC(s) }

    def protoTail =
      rep(astAttr) ~ opt(astSection) ~ opt(astAlign) ~ opt(astGC) ^^
        { case attrs ~ s ~ align ~ gc => (attrs, s, align, gc) }

    protoHead ~ astFormalParamList ~ protoTail ^^
      { case h ~ m ~ t => FunctionHeader(h._1, h._2, h._3, h._4, h._6, FunType(h._5, m, t._1), t._2, t._3, t._4) }
  }


  def addBlockMap(id:Int, b:Block) {
    val m1 = funBlockMap.get(currentFun) match {
      case Some(m) => m
      case None => Map[Int, Block]()
    }
    funBlockMap += (currentFun -> (m1 + (id -> b)))
  }

  def astBlock:Parser[Block] =
    opt(astLabelDef) ~ rep(astInstruction) ~ astControlInstWithDbg ^^
      { case l ~ insts ~ ctrlInst => {
        blockSerno += 1
        val block = Block(l, blockSerno, insts, ctrlInst);
        addBlockMap(blockSerno, block)
        block
      } }

  def astDeclare:Parser[Declare] =
    "declare" ~ astFunctionHeader ^^ { case "declare" ~ header => Declare(header) }

  def astDefine:Parser[FunctionDef] =
    ("define" ~> astFunctionHeader ~ "{" ~ rep(astBlock) ~ "}" ^^
      { case fproto ~ "{" ~ blocks ~ "}" =>  FunctionDef(fproto, blocks) }
      | "define" ~> astFunctionHeader ~ "begin" ~ rep(astBlock) ~ "end" ^^
      { case fproto ~ "begin" ~ blocks ~ "end" =>  FunctionDef(fproto, blocks) })

  def astModuleAsm = "module" ~ "asm" ~ quoteStr ^^ { case "module" ~ "asm" ~ str => ModuleAsm(str) }

  def astTargetDefinition:Parser[Target] =
    "target" ~ ("triple" | "datalayout") ~ "=" ~ quoteStr ^^ { case "target" ~ x ~ "=" ~ str => Target(x, str) }

  def astDepLibs:Parser[DepLibs] =
    "deplibs"  ~ "=" ~ "[" ~ repsep (quoteStr, ",") ~ "]" ^^ { case "deplibs" ~ "=" ~ "[" ~ l ~ "]" => DepLibs(l) }


  def astTypeDef:Parser[TopLevel] = {
    def astTypeDefLHS:Parser[Any] =
      (localVarOrID ~ "=" ~ "type" ^^ { case x ~ "=" ~ "type" => namedTypes += x.toString; x.toString }
        | quoteStr ~ "=" ~ "type" ^^ { case n ~ "=" ~ "type" => n.toString }
        | "type" ^^
        {
          case "type" => {
            val id = unnamedTypeCount
            unnamedTypes += "%" + unnamedTypeCount.toString;
            unnamedTypeCount += 1;
            id
          }
        })

    astTypeDefLHS ~ astType ^^ {
      case (name:String) ~ rhs => TypeDef(name, rhs)
      case (id:Int) ~ rhs => UnamedType(id, rhs)
      case x ~ rhs => UnamedType(-1, rhs) // this should not happen, add it to remove warning
    }
  }

  def astGlobalType = ("constant" | "global") ^^ { case x => GlobalType (x) }

  def astAddrSpace = "addrspace" ~ "(" ~ numericLit ~ ")" ^^
    { case "addrspace" ~ "(" ~ n ~ ")" => AddrSpace(Integer.parseInt(n)) }

  def astGlobalDef = {
    def astAliasee =
      (astTV ^^ { case tv => AliaseeTV(tv) }
        | astConstCast ^^ { case ConstCast(conv, tv, t) => AliaseeBitCast(tv, t) }
        | astConstGetElemPtr ^^ { case e => AliaseeGetElemPtr(e) })

    (opt(globalVarOrID <~ "=") ~ opt(astVisibility) ~ "alias" ~ opt(astLinkage) ~ astAliasee ^^
      { case  lhs ~ v ~ "alias" ~ l ~ alias => Alias (lhs, v, l, alias) }

      | opt(globalVarOrID <~ "=") ~ astExternLinkage ~ opt(astVisibility) ~ opt("thread_local")
      ~ opt(astAddrSpace) ~ astGlobalType ~ astType ~ opt("," ~> astAlign) ^^
      { case lhs ~ l ~ v ~ t ~ a ~ g ~ ty ~ align => Global(lhs, Some(l), v, t.isDefined, a, g, ty, None, None, align) }

      | opt(globalVarOrID <~ "=") ~ opt(astLinkage) ~ opt(astVisibility) ~ opt("thread_local")
      ~ opt(astAddrSpace) ~ astGlobalType ~ astType ~ opt(astConst)  ~ opt("," ~> astSection) ~ opt("," ~> astAlign) ^^
      { case lhs ~ l ~ v ~ t ~ a ~ g ~ ty ~ const ~ section ~ align =>
        Global (lhs, l, v, t.isDefined, a, g, ty, const, section, align)
      })
  }


  def astStandardaloneMD:Parser[StandardaloneMD] =
    "!" ~ numericLit ~ "=" ~ astTV ^^ { case "!" ~ n ~ "=" ~ tv => StandardaloneMD (n, tv) }

  def astNamedMD:Parser[NamedMD] =
    mdVar ~ "=" ~ "!" ~ "{" ~ repsep (astMDNode, ",")  ~ "}" ^^
      { case mv ~ "=" ~ "!" ~ "{" ~ nodes ~ "}" => NamedMD (mv, nodes) }

  def astDbgInit =
    "!" ~ ident ~ "=" ~ "!" ~ "{" ~ "!" ~ numericLit ~ "}" ^^
      { case "!" ~ ident ~ "=" ~ "!" ~ "{" ~ "!" ~ n ~ "}" => DbgInit (ident, Integer.parseInt(n)) }

  def astDecl:Parser[TopLevel] =
    (astDepLibs | astDefine | astDeclare | astModuleAsm | astTargetDefinition | astTypeDef
      | astGlobalDef | astStandardaloneMD | astNamedMD | astDbgInit)

  def astTop:Parser[Module] =
    rep(astDecl) ^^ { case l => Module(l) }

  def parse(s:Reader[Char]):Option[(Module, Map[GlobalVarOrID, Map[Int, Block]])] = {
    val tokens = new lexical.Scanner(s)
    val result = phrase(astTop)(tokens)
    result match {
      case Success(out, _) => Some((out, funBlockMap))
      case _ => Console.println(result); None
    }
  }
}