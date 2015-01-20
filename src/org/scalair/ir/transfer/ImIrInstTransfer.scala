package org.scalair.ir.transfer

import org.scalair.hoopl._
import org.scalair.hoopl.TypeDefines._
import collection.immutable.HashMap
import org.scalair.ir.imir._

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


trait ImIrInstTransfer[F<:Ordered[F]] extends ImIrValueTransfer[F] {
  import ET._
  val vis: InstVisitor[F]

  def clRHS(fact:F)(x:ET[RHS]):F = if (vis.visitRHS(fact)(x)) {
    val fact1 = vis.bcRHS(fact)(x)
    val fact2 = asT(x) match {
      case e: Expr => clExpr(fact1)(asA(x,e))
      case e: MemOp => clMemOp(fact1)(asA(x,e))
      case Phi(ty, ins) => {
        val fact2 = clType(fact1)(asA(x,ty))
        ins.foldLeft(fact2) { (p, v) =>
          val fact3 = clValue(p)(asA(x,v._1))
          clValue(fact3)(asA(x,v._2))
        }
      }
      case Call(cconv, retAttrs, retType, fn, actualParams, funAttrs) => {
        val fact2 = clType(fact1)(asA(x,retType));
        val fact3 = clVarOrID(fact2)(asA(x,fn));
        clActualParamList(fact3)(asA(x,actualParams))
      }
      case ExtractElement(tv, index) => {
        val fact2 = clValue(fact1)(asA(x,tv))
        clValue(fact2)(asA(x,index))
      }
      case InsertElement(vect, tv, index) => {
        val fact2 = clValue(fact1)(asA(x,vect))
        val fact3 = clValue(fact2)(asA(x,tv))
        clValue(fact3)(asA(x,index))
      }
      case ShuffleVector(vect1, vect2, mask) => {
        val fact2 = clValue(fact1)(asA(x,vect1))
        val fact3 = clValue(fact2)(asA(x,vect2))
        clValue(fact3)(asA(x,mask))
      }
      case ExtractValue(tv, indices) => clValue(fact1)(asA(x,tv))
      case InsertValue(vect, tv, indices) => {
        val fact2 = clValue(fact1)(asA(x,vect))
        clValue(fact2)(asA(x,tv))
      }
      case VaArg(tv, v) => {
        val fact2 = clValue(fact1)(asA(x,tv))
        clType(fact2)(asA(x,v))
      }
    }
    vis.acRHS(fact2)(x)
  } else fact

  def clExpr(fact:F)(x:ET[Expr]):F = if (vis.visitExpr(fact)(x)) {
    val fact1 = vis.bcExpr(fact)(x)
    val fact2 = asT(x) match {
      case ArithmaticExpr(binOp, carry, t, v1, v2) => {
        val fact2 = clType(fact1)(asA(x, t))
        val fact3 = clValue(fact2)(asA(x, v1))
        clValue(fact3)(asA(x, v2))
      }
      case ConstExpr(const) => clConst(fact)(asA(x,const))
      case ICmpExpr(cmp, t, v1, v2) => {
        val fact2 = clType(fact1)(asA(x, t))
        val fact3 = clValue(fact2)(asA(x, v1))
        clValue(fact3)(asA(x,v2))
      }
      case FCmpExpr(cmp, t, v1, v2) => {
        val fact2 = clType(fact1)(asA(x,t))
        val fact3 = clValue(fact2)(asA(x,v1))
        clValue(fact3)(asA(x,v2))
      }
      case BitwiseExpr(binOp, t, v1, v2) => {
        val fact2 = clType(fact1)(asA(x,t))
        val fact3 = clValue(fact2)(asA(x,v1))
        clValue(fact3)(asA(x,v2))
      }
      case GetElemPtr(tv, indices) => {
        val fact2 = clValue(fact1)(asA(x,tv))
        indices.foldLeft(fact2) { (p, v) => clConst(p)(asA(x,v)) }
      }
      case GetResult(tv, index) => clValue(fact)(asA(x,tv))
      case CastExpr(op, tv, t) => {
        val fact2 = clValue(fact1)(asA(x,tv))
        clType(fact2)(asA(x,t))
      }
      case Select(c, t, v) => {
        val fact2 = clValue(fact1)(asA(x,c))
        val fact3 = clValue(fact2)(asA(x,t))
        clValue(fact3)(asA(x,v))
      }
      case Extract(tv, idx) => clValue(fact1)(asA(x,tv))
    }
    vis.acExpr(fact2)(x)
  } else fact

  def clMemOp(fact:F)(x:ET[MemOp]):F = if (vis.visitMemOp(fact)(x)) {
    val fact1 = vis.bcMemOp(fact)(x)
    val fact2 = asT(x) match {
      case Alloca(ty, size, align) => {
        val fact2 = clType(fact1)(asA(x,ty))
        size.foldLeft(fact2) { (p, v) => clValue(p)(asA(x,v)) }
      }
      case Malloc(ty, size, align) => {
        val fact2 = clType(fact1)(asA(x,ty))
        size.foldLeft(fact2) { (p, v) => clValue(p)(asA(x,v)) }
      }
      case Free(tv) => clValue(fact1)(asA(x,tv))
      case Load(volatile, ptr, align) => clValue(fact1)(asA(x,ptr))
      case Store(volatile, v, ptr, align) => {
        val fact2 = clValue(fact1)(asA(x,v))
        clValue(fact2)(asA(x,ptr))
      }
    }
    vis.acMemOp(fact2)(x)
  } else fact

  def clInstruction(fact:F)(x:ET[AbsInst]):F = if (vis.visitInstruction(fact)(x)) {
    val fact1 = vis.bcInstruction(fact)(x)
    val fact2 = asT(x) match {
      case y: Instruction => {
        val fact3 = y.lhs.foldLeft(fact1) { (p, v) => clVarOrID(p)(asA(x,v)) }
        clRHS(fact3)(asA(x,y.rhs))
      }
      case Comment(s) => fact1
    }
    vis.acInstruction(fact2)(x)
  } else fact

  def clControlInst(fact:F)(x:ET[ControlInst]):F = if (vis.visitControlInst(fact)(x)) {
    val fact1 = vis.bcControlInst(fact)(x)
    val fact2 = asT(x) match {
      case Unreachable() => fact1
      case Ret(v) => v.foldLeft(fact1) { (p,v) => clValue(p)(asA(x,v)) }
      case Br(l) => clValue(fact1)(asA(x,l))
      case CBr(cond, tru, fal) => {
        val fact2 = clValue(fact1)(asA(x,cond))
        val fact3 = clValue(fact2)(asA(x,tru))
        clValue(fact3)(asA(x,fal))
      }
      case IndirectBr(cond, branchs) => {
        val fact2 = clValue(fact1)(asA(x,cond))
        branchs.foldLeft(fact2) { (p,v) => clValue(p)(asA(x,v)) }
      }
      case Switch(v, default, jumptable) => {
        val fact2 = clValue(fact1)(asA(x,v))
        val fact3 = clValue(fact2)(asA(x,default))
        jumptable.foldLeft(fact3) { (p, t) =>
          val fact4 = clValue(p)(asA(x,t._1))
          clValue(fact4)(asA(x,t._2))
        }
      }
      case Invoke(lhs, cconv, retAttrs, fn, actualParams, funAttrs, label, exception) => {
        val fact2 = lhs.foldLeft(fact1) { (p,v) => clVarOrID(p)(asA(x,v)) }
        val fact3 = clValue(fact2)(asA(x,fn))
        val fact4 = clActualParamList(fact3)(asA(x,actualParams))
        val fact5 = clValue(fact4)(asA(x,label))
        clValue(fact5)(asA(x,exception))
      }
      case Unwind() => fact1
    }
    vis.acControlInst(fact2)(x)
  } else fact

  def clControlInstDbg(fact:F)(x:ET[ControlInstDbg]):F = if (vis.visitControlInstDbg(fact)(x)) {
    val fact1 = vis.bcControlInstDbg(fact)(x)
    val fact2 = clControlInst(fact1)(asA(x, asT(x).inst))
    val fact3 = asT(x).dbg.foldLeft(fact2) { (p,v) => clDbg(p)(asA(x,v)) }
    vis.acControlInstDbg(fact3)(x)
  } else fact


  def clDbg(fact:F)(x:ET[Dbg]):F = if (vis.visitDbg(fact)(x)) {
    val fact1 = vis.bcDbg(fact)(x)
    val fact2 = clMetaConst(fact1)(asA(x, asT(x).meta))
    vis.acDbg(fact2)(x)
  } else fact




  def clActualParam(fact:F)(x:ET[ActualParam]):F = if (vis.visitActualParam(fact)(x)) {
    val fact1 = vis.bcActualParam(fact)(x)
    val fact2 = clValue(fact1)(asA(x, asT(x).v))
    vis.acActualParam(fact2)(x)
  } else fact

  def clActualParamList(fact:F)(x:ET[ActualParamList]):F = if (vis.visitActualParamList(fact)(x)) {
    val fact1 = vis.bcActualParamList(fact)(x)
    val fact2 = asT(x).list.foldLeft(fact1) { (p, v) => clActualParam(p)(asA(x,v)) }
    vis.acActualParamList(fact2)(x)
  } else fact


  trait InstVisitor[F] extends ValueVisitor[F] {

    def visitRHS(fact: F)(x: ET[RHS]): Boolean = true
    def bcRHS(fact: F)(x: ET[RHS]): F = fact
    def acRHS(fact: F)(x: ET[RHS]): F = fact

    def visitExpr(fact: F)(x: ET[Expr]): Boolean = true
    def bcExpr(fact: F)(x: ET[Expr]): F = fact
    def acExpr(fact: F)(x: ET[Expr]): F = fact

    def visitMemOp(fact: F)(x: ET[MemOp]): Boolean = true
    def bcMemOp(fact: F)(x: ET[MemOp]): F = fact
    def acMemOp(fact: F)(x: ET[MemOp]): F = fact

    def visitInstruction(fact: F)(x: ET[AbsInst]): Boolean = true
    def bcInstruction(fact: F)(x: ET[AbsInst]): F = fact
    def acInstruction(fact: F)(x: ET[AbsInst]): F = fact

    def visitControlInst(fact: F)(x: ET[ControlInst]): Boolean = true
    def bcControlInst(fact: F)(x: ET[ControlInst]): F = fact
    def acControlInst(fact: F)(x: ET[ControlInst]): F = fact

    def visitControlInstDbg(fact: F)(x: ET[ControlInstDbg]): Boolean = true
    def bcControlInstDbg(fact: F)(x: ET[ControlInstDbg]): F = fact
    def acControlInstDbg(fact: F)(x: ET[ControlInstDbg]): F = fact

    def visitAbsNodeToLabel(fact: F)(n: AbsNode): Boolean = true
    def bcAbsNodeToLabel(fact: F)(n: AbsNode): F = fact
    def acAbsNodeToLabel(fact: F)(n: AbsNode): F = fact

    def visitBlock(fact: F)(x: Block[AbsNode, C, C]): Boolean = true
    def bcBlock(fact: F)(x: Block[AbsNode, C, C]): F = fact
    def acBlock(fact: F)(x: Block[AbsNode, C, C]): F = fact

    def visitActualParam(fact: F)(x: ET[ActualParam]): Boolean = true
    def bcActualParam(fact: F)(x: ET[ActualParam]): F = fact
    def acActualParam(fact: F)(x: ET[ActualParam]): F = fact

    def visitActualParamList(fact: F)(x: ET[ActualParamList]): Boolean = true
    def bcActualParamList(fact: F)(x: ET[ActualParamList]): F = fact
    def acActualParamList(fact: F)(x: ET[ActualParamList]): F = fact

    def visitDbg(fact: F)(x: ET[Dbg]): Boolean = true
    def bcDbg(fact: F)(x: ET[Dbg]): F = fact
    def acDbg(fact: F)(x: ET[Dbg]): F = fact
  }
}