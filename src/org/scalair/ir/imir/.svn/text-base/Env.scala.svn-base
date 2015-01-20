package org.scalair.ir.imir

import org.scalair.ir.common.ConvOp

/**
 * User: wangn
 * Date: 4/22/11
 */



case class RtMetaData(typeCode:Value, baseAddr:Value, size:Option[Value]=None)

case class TripleValue(dataVal:Value, rtMetaData:RtMetaData)

class Env(override val localDef:EntityDefMap[Value],
          override val labelDef:EntityDefMap[Int],
          override val globalDef:EntityDefMap[Value],
          override val typeDef:TypeEntityDefMap,
          override val toeTbl:ValueInfoMap) extends BaseEnv(localDef, labelDef, globalDef, typeDef, toeTbl)
{
  def this(e:BaseEnv) = this(e.localDef, e.labelDef, e.globalDef, e.typeDef, e.toeTbl)

  var replacement = Map[Value, Value]()

  var ptrToSizeLoc:Map[Value, Value] = Map()
  var ptrToSize:Map[Value, Value] = Map()
  var strToFileInfo:Map[String, Const] = Map()
  var strToCoord:Map[String, Const] = Map()
  var ptrToRtMetaData:Map[Value, RtMetaData] = Map()

  var funSigs:Map[String, Const] = Map()

  var hasRewritten = Map[Value, Boolean]()
  var globalScope = Map[Value, Boolean]()
  var instIsRewritten = Map[Value, Boolean]()

  var toPatchUp = Map[Phi, Int]()

  var renamed = Map[FunctionDef, Boolean]()
  var allocaToStaticPPInfoMap = Map[Value, GlobalVarOrID]()
  var globalVariableToStaticPPInfoMap = Map[GlobalVarOrID, Const]()
  var strToStrStoragePtr = Map[String, Const]()
  var generatedGlobalVairables = Map[GlobalVarOrID, Boolean]()
  var strToConstant = Map[String, Const]()
  var oldGVToNewGV = Map[GlobalVarOrID, GlobalVarOrID]()
  var oldGVToNewGVMemHolder = Map[Value, Const]()
  var newGVMemHolderSize = Map[Value, Const]()
  var newGVMemHolderType = Map[Value, Type]()
  var oldGVToElementType = Map[GlobalVarOrID, Type]()
  var oldGVToMetaType = Map[GlobalVarOrID, Type]()
  //var defSites = Map[Value, Block]()
  var erasedCallInsts = Map[Instruction, Boolean]()
  var normalFunctions = List[Call]()
  var callSiteSign = Map[Call, Const]()



  def addReplace(o:Value, n:Value) = {
    replacement += (o -> n)
  }


  def getReplacement(o:Value) = o match {
    case x:VarOrID => replacement.getOrElse(x, x)
    case _ => o
  }
  def curStackPType = PrimitiveConst(I(32), IntConst("0"))
}