package org.scalair.ir.rewriter

import org.scalair.hoopl._
import org.scalair.ir.imir._

/**
 * User: wangn
 * Date: Mar 8, 2011
 */


trait ImIrInstRewriter[S,F<:Ordered[F]] extends ImIrValueRewriter[S,F] {

  def rwPhi(env:F)(x:A[Phi]):A[Phi] = x
  def rwCall(env:F)(x:A[Call]):A[Call] = x
  def rwExtractElement(env:F)(x:A[ExtractElement]):A[ExtractElement] = x
  def rwInsertElement(env:F)(x:A[InsertElement]):A[InsertElement] = x
  def rwShuffleVector(env:F)(x:A[ShuffleVector]):A[ShuffleVector] = x
  def rwExtractValue(env:F)(x:A[ExtractValue]):A[ExtractValue] = x
  def rwInsertValue(env:F)(x:A[InsertValue]):A[InsertValue] = x
  def rwVaArg(env:F)(x:A[VaArg]):A[VaArg] = x

  def rwRHS(env:F)(x:A[RHS]):A[RHS] = x flatMap {
    _ match {
      case Left(e:Expr) => rwExpr(env)(left(e))
      case Left(e:MemOp) => rwMemOp(env)(left(e))
      case Left(e:Phi) => rwPhi(env)(left(e))
      case Left(e:Call) => rwCall(env)(left(e))
      case Left(e:ExtractElement) => rwExtractElement(env)(left(e))
      case Left(e:InsertElement) => rwInsertElement(env)(left(e))
      case Left(e:ShuffleVector) => rwShuffleVector(env)(left(e))
      case Left(e:ExtractValue) => rwExtractValue(env)(left(e))
      case Left(e:InsertValue) => rwInsertValue(env)(left(e))
      case Left(e:VaArg) => rwVaArg(env)(left(e))
    }
  }

  def rwArithmaticExpr(env:F)(x:A[ArithmaticExpr]):A[ArithmaticExpr] = x
  def rwConstExpr(env:F)(x:A[ConstExpr]):A[ConstExpr] = x
  def rwICmpExpr(env:F)(x:A[ICmpExpr]):A[ICmpExpr] = x
  def rwFCmpExpr(env:F)(x:A[FCmpExpr]):A[FCmpExpr] = x
  def rwBitwiseExpr(env:F)(x:A[BitwiseExpr]):A[BitwiseExpr] = x
  def rwGetElemPtr(env:F)(x:A[GetElemPtr]):A[GetElemPtr] = x
  def rwGetResult(env:F)(x:A[GetResult]):A[GetResult] = x
  def rwCastExpr(env:F)(x:A[CastExpr]):A[CastExpr] = x
  def rwSelect(env:F)(x:A[Select]):A[Select] = x
  def rwExtract(env:F)(x:A[Extract]):A[Extract] = x

  def rwExpr(env:F)(x:A[Expr]):A[Expr] = x flatMap {
    _ match {
      case Left(e:ArithmaticExpr) => rwArithmaticExpr(env)(left(e))
      case Left(e:ConstExpr) => rwConstExpr(env)(left(e))
      case Left(e:ICmpExpr) => rwICmpExpr(env)(left(e))
      case Left(e:FCmpExpr) => rwFCmpExpr(env)(left(e))
      case Left(e:BitwiseExpr) => rwBitwiseExpr(env)(left(e))
      case Left(e:GetElemPtr) => rwGetElemPtr(env)(left(e))
      case Left(e:GetResult) => rwGetResult(env)(left(e))
      case Left(e:CastExpr) => rwCastExpr(env)(left(e))
      case Left(e:Select) => rwSelect(env)(left(e))
      case Left(e:Extract) => rwExtract(env)(left(e))
    }
  }

  def rwAlloca(env:F)(x:A[Alloca]):A[Alloca] = x
  def rwMalloc(env:F)(x:A[Malloc]):A[Malloc] = x
  def rwFree(env:F)(x:A[Free]):A[Free] = x
  def rwLoad(env:F)(x:A[Load]):A[Load] = x
  def rwStore(env:F)(x:A[Store]):A[Store] = x

  def rwMemOp(env:F)(x:A[MemOp]):A[MemOp] = x flatMap {
    _ match {
      case Left(e:Alloca) => rwAlloca(env)(left(e))
      case Left(e:Malloc) => rwMalloc(env)(left(e))
      case Left(e:Free) => rwFree(env)(left(e))
      case Left(e:Load) => rwLoad(env)(left(e))
      case Left(e:Store) => rwStore(env)(left(e))
    }
  }

  def rwInstruction(env:F)(x:A[AbsInst]):A[AbsInst] = x flatMap {
    _ match {
      case Left(e:Instruction) => x
      case Left(e:Comment) => x
    }
  }

  def rwControlInst(env:F)(x:A[ControlInst]):A[ControlInst] = x flatMap {
    _ match {
      case Left(e:Unreachable) => x
      case Left(e:Ret) => x
      case Left(e:Br) => x
      case Left(e:CBr) => x
      case Left(e:IndirectBr) => x
      case Left(e:Switch) => x
      case Left(e:Invoke) => x
      case Left(e:Unwind) => x
    }
  }

  /*
  def rwControlInstDbg(env:F)(x:A[ControlInstDbg]):A[ControlInstDbg] = x flatMap {
    _ match {
      case Left(ControlInstDbg(inst, dbg)) => rwControlInst(env)(left(inst))
    }
  }
  */


  def rwDbg(env:F)(x:A[Dbg]):A[Dbg] = x

  /*
  def rwAbsNode[E,X](env:F)(x: A[AbsNode]):CMM#M[Option[Graph[A[AbsNode],E,X]]] = x flatMap {
    _ match {
      case Left(FirstNode(l, e)) => cmObj._return(None)
      case Left(MiddleNode(n)) => {
        //rwInstruction(env)(left(n))
        cmObj._return(None)
      }
      case Left(LastNode(targets, n)) => {
        //rwControlInst(env)(left(n))
        cmObj._return(None)
      }
      case _ => throw new Exception()
    }
  }

  def rwLastNode(env:F)(x:A[LastNode]):Fact[C,F] = {
    val env1 = rwControlInst(env)(asA(x, asT(x).inst))
    var mp = new HashMap[BlockId, F]()
    for (l <- asT(x).targets) mp = mp + (l -> env1)
    Right(mp)
  }

  def first:Block[A[AbsNode],C,O]=>F=>CMM#M[Option[Graph[A[AbsNode],C,O]]] = { b => fact =>
    b match {
      case BFirst(n) => rwAbsNode(fact)(n)
      case _ => throw new Exception()
    }
  }

  def middle:Block[A[AbsNode],O,O]=>F=>CMM#M[Option[Graph[A[AbsNode],O,O]]] = { b => fact =>
    b match {
      case BMiddle(n) => rwAbsNode(fact)(n)
      case _ => throw new Exception()
    }
  }

  def last:Block[A[AbsNode],O,C]=>F=>CMM#M[Option[Graph[A[AbsNode],C,O]]] = { b => fact =>
    b match {
      case BLast(n) => rwLastNode(fact)(n.asInstanceOf[A[LastNode]])
      case _ => throw new Exception ()
    }
  }
  */

  def rwActualParam(env:F)(x:A[ActualParam]):A[ActualParam] = x
  def rwActualParamList(env:F)(x:A[ActualParamList]):A[ActualParamList] = x
}