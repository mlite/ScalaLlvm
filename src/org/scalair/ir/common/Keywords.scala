package org.scalair.ir.common

import org.scalair.ir.util.Emitter

/**
 * User: wangn
 * Date: 4/17/11
 */

// Attribute
sealed abstract class Attr(val name:String)  extends Emitter { override def toString = name }
case object ZeroExt extends Attr("zeroext")
case object SignExt extends Attr("signext")
case object InReg extends Attr("inreg")
case object ByVal extends Attr("byval")
case object SRet extends Attr("sret")
case object NoAlias extends Attr("noalias")
case object NoCapture extends Attr("nocapture")
case object Nest extends Attr("nest")

sealed abstract class FunAttr(override val name:String) extends Attr(name)
case class AlignStack(val n:Int) extends FunAttr("alignstack(" + n + ")")
case object AlwaysInline extends FunAttr("alwaysinline")
case object HotPatch extends FunAttr("hotpatch")
case object InlineHint extends FunAttr("inlinehint")
case object Naked extends FunAttr("naked")
case object NoImplicitFloat extends FunAttr("noimplicitfloat")
case object NoInline extends FunAttr("noinline")
case object NoRedzone extends FunAttr("noredzone")
case object NoReturn extends FunAttr("noreturn")
case object NoUnwind extends FunAttr("nounwind")
case object OptSize extends FunAttr("optsize")
case object ReadNone extends FunAttr("readnone")
case object ReadOnly extends FunAttr("readonly")
case object Ssp extends FunAttr("ssp")
case object SspReq extends FunAttr("sspreq")
case object SideEffect extends FunAttr("sideeffect")


// Carray
sealed abstract class Carry(val name:String) extends Emitter { override def toString = name }
case object Nuw extends Carry("nuw")
case object Nsw extends Carry("nsw")
case object Exact extends Carry("exact")

// Call Convention
sealed abstract class CConv(val name:String) extends Emitter { override def toString = name }
case object CCC extends CConv("ccc")
case object Fast extends CConv("fastcc")
case object Cold extends CConv("coldcc")
case object X86_stdcall extends CConv("x86_stdcallcc")
case object X86_fastcall extends CConv("x86_fastcallcc")
case object X86_thiscall extends CConv("x86_thiscallcc")
case object Arm_apcs extends CConv("arm_apcscc")
case object Arm_aapcs extends CConv("arm_aapcscc")
case object Arm_aapcs_vfp extends CConv("arm_aapcs_vfpcc")
case object Msp430_intr extends CConv("msp430_intrcc")
case class CC(val s:String) extends CConv("cc"+s)


// Linkage
sealed abstract class Linkage(val name:String) extends Emitter { override def toString = name }
case object Private extends Linkage("private")
case object Linker_private extends Linkage("linker_private")
case object Linker_private_weak extends Linkage("linker_private_weak")
case object Linker_private_weak_def_auto extends Linkage("linker_private_weak_def_auto")
case object Internal extends Linkage("internal")
case object External extends Linkage("external")
case object Available_externally extends Linkage("available_externally")
case object Linkonce extends Linkage("linkonce")
case object Weak extends Linkage("weak")
case object Common extends Linkage("common")
case object Appending extends Linkage("appending")
case object Extern_weak extends Linkage("extern_weak")
case object Linkonce_odr extends Linkage("linkonce_odr")
case object Weak_odr extends Linkage("weak_odr")
case object Dllimport extends Linkage("dllimport")
case object Dllexport extends Linkage("dllexport")


// Visibility
sealed abstract class Visibility(val name:String) extends Emitter { override def toString = name }
case object Default extends Visibility("default")
case object Hidden extends Visibility("hidden")
case object Protected extends Visibility("protected")

// Misc
case class Align(n:Int) extends Emitter { override def toString = "align " + n; }
case class Section(s:QuoteStr) extends Emitter { override def toString = "section " + s }
case class GC(val name:QuoteStr) extends Emitter { override def toString = "gc " + name }
case class GlobalType(s:String) { override def toString = s }
case class AddrSpace(n:Int) extends Emitter { override def toString = "addrspace(" + n + ")" }


// Operator
sealed abstract class Operator(val name:String) extends Emitter { override def toString = name; }
sealed abstract class ArithmaticOp(override val name:String) extends Operator(name)
case object Add extends ArithmaticOp("add")
case object FAdd extends ArithmaticOp("fadd")
case object Sub extends ArithmaticOp("sub")
case object FSub extends ArithmaticOp("fsub")
case object Mul extends ArithmaticOp("mul")
case object FMul extends ArithmaticOp("fmul")
case object UDiv extends ArithmaticOp("udiv")
case object SDiv extends ArithmaticOp("sdiv")
case object FDiv extends ArithmaticOp("fdiv")
case object URem extends ArithmaticOp("urem")
case object SRem extends ArithmaticOp("srem")
case object FRem extends ArithmaticOp("frem")

sealed abstract class BitwiseOp(override val name:String) extends Operator(name)
case object Shl extends BitwiseOp("shl")
case object LShr extends BitwiseOp("lshr")
case object AShr extends BitwiseOp("ashr")
case object And extends BitwiseOp("and")
case object Or extends BitwiseOp("or")
case object Xor extends BitwiseOp("xor")

sealed abstract class FCmpOp(override val name:String) extends Operator(name)
case object FCmpOeq extends FCmpOp("oeq")
case object FCmpOne extends FCmpOp("one")
case object FCmpOlt extends FCmpOp("olt")
case object FCmpOgt extends FCmpOp("ogt")
case object FCmpOle extends FCmpOp("ole")
case object FCmpOge extends FCmpOp("oge")
case object FCmpOrd extends FCmpOp("ord")
case object FCmpUno extends FCmpOp("uno")
case object FCmpUeq extends FCmpOp("ueq")
case object FCmpUne extends FCmpOp("une")
case object FCmpUlt extends FCmpOp("ult")
case object FCmpUgt extends FCmpOp("ugt")
case object FCmpUle extends FCmpOp("ule")
case object FCmpUge extends FCmpOp("uge")
case object FCmpTrue extends FCmpOp("true")
case object FCmpFalse extends FCmpOp("false")

sealed abstract class ICmpOp(override val name:String) extends Operator(name)
case object ICmpEq extends ICmpOp("eq")
case object ICmpNe extends ICmpOp("ne")
case object ICmpSlt extends ICmpOp("slt")
case object ICmpSgt extends ICmpOp("sgt")
case object ICmpSle extends ICmpOp("sle")
case object ICmpSge extends ICmpOp("sge")
case object ICmpUlt extends ICmpOp("ult")
case object ICmpUgt extends ICmpOp("ugt")
case object ICmpUle extends ICmpOp("ule")
case object ICmpUge extends ICmpOp("uge")


sealed abstract class ConvOp (override val name:String) extends Operator(name)
case object Trunc extends ConvOp("trunc")
case object ZExt extends ConvOp("zext")
case object SExt extends ConvOp("sext")
case object FPTrunc extends ConvOp("fptrunc")
case object FPExt extends ConvOp("fpext")
case object FPtoUI extends ConvOp("fptoui")
case object FPtoSI extends ConvOp("fptosi")
case object UItoFP extends ConvOp("uitofp")
case object SItoFP extends ConvOp("sitofp")
case object PtrtoInt extends ConvOp("ptrtoint")
case object InttoPtr extends ConvOp("inttoptr")
case object Bitcast extends ConvOp("bitcast")


// String Value
sealed abstract class StrVal(val content:String) extends Emitter { override def toString = content }
case class QuoteStr(val name:String) extends StrVal("\"" + name + "\"") {
}
case class PlainStr(val name:String) extends StrVal(name)