package org.scalair.ir.imir

/**
 * User: wangn
 * Date: 4/22/11
 */

class BaseEnv(val localDef:EntityDefMap[Value],
              val labelDef:EntityDefMap[Int],
              val globalDef:EntityDefMap[Value],
              val typeDef:TypeEntityDefMap,
              val toeTbl:ValueInfoMap)
{

  def this(d:DataLayout) = this(new EntityDefMap(), new EntityDefMap(), new EntityDefMap(), new TypeEntityDefMap(d), new ValueInfoMap())
  def this(dt:BaseEnv) = this(dt.localDef, dt.labelDef, dt.globalDef, dt.typeDef, dt.toeTbl)

  def addLocalDef(s:Int, rhs:Value) = localDef.addDef(s, rhs)
  def addLocalDef(s:String, rhs:Value) = localDef.addDef(s, rhs)
  def addGlobalDef(s:Int, rhs:Value) = globalDef.addDef(s, rhs)
  def addGlobalDef(s:String, rhs:Value) = globalDef.addDef(s, rhs)
  def addTypeDef(s:Int, rhs:Type) = typeDef.addDef(s, rhs)
  def addTypeDef(s:String, rhs:Type) = typeDef.addDef(s, rhs)

  def add(v:Value, te:Type) = toeTbl.add(v, te)
  def add(v:Option[Value], te:Type) = toeTbl.add(v, te)

  private def insert[T <: Value](v:T, te:Type):T = { toeTbl.add(v, te); v }

  def getNewGlobalID(te:Type, rhs:Option[Value]=None):GlobalID  = {
    val rhs0 = rhs.getOrElse(PrimitiveConst(te, Undef()))
    insert(GlobalID(globalDef.getNewID(rhs0)), te)
  }
  def getNewGlobalVar(s:String, te:Type, rhs:Option[Value]=None):GlobalVar = {
    val rhs0 = rhs.getOrElse(PrimitiveConst(te, Undef()))
    insert(GlobalVar(globalDef.getNewName(s, rhs0)), te)
  }

  def getNewLocalID(te:Type, rhs:Option[Value]=None):LocalID = {
    val rhs0 = rhs.getOrElse(PrimitiveConst(te, Undef()))
    insert(LocalID(localDef.getNewID(rhs0)), te)
  }

  def getNewLocalVar(s:String, te:Type, rhs:Option[Value]=None):LocalVar = {
    val rhs0 = rhs.getOrElse(PrimitiveConst(te, Undef()))
    insert(LocalVar(localDef.getNewName(s, rhs0)), te)
  }


  def typeOf(c:Const):Type = c match {
    case StructConst(fields, isPacked) => StructType(fields.map(typeOf(_)), isPacked)
    case ArrayConst(fields) => ArrayType(fields.length, typeOf(fields.head))
    case VectorConst(fields) => VectorType(fields.length, typeOf(fields.head))
    case PrimitiveConst(t, _) => t
    case ConstCast(_,_,t) => t
    case ConstArithmaticExpr(_,_,op1,op2) => typeOf(op1)
    case ConstFCmpExpr(_,_,_) => I(1)
    case ConstICmpExpr(_,_,_) => I(1)
    case MDRef(n) => toeTbl.getType(n).get
    //case x:VarOrID => toeTbl.getType(c).get
    case ConstGetElemPtr(_, base, indices) => typeOf(typeOf(base), indices)
    case _ => throw new Exception("typeOf " + c)
  }

  def sizeOfInBit(t:Type):Int = typeDef.sizeOfInBit(t)
  def sizeofInByte(t:Type):Int = typeDef.sizeOfInByte(t)
  def sizeOfInByte(ty:Type):PrimitiveConst = PrimitiveConst(I(32), SizeOf(ty))

  def typeOf(t:Type, indices:List[Const]):Type = {
    def innerteof(t:Type, indices:List[Const]):Type = indices match {
      case List() => t
      case PrimitiveConst(_, IntConst(n))::tail => t match {
        case PointerType(b,_) => typeOf(b, tail)
        case StructType(l,_) => innerteof(l(Integer.parseInt(n)), tail)
        case ArrayType(n,b)=> innerteof(b, tail)
        case x => throw new Exception("bad getelementptr " + t + " " + indices)
      }
      case _ => throw new Exception("bad getelementptr " + t + " " + indices)
    }
    (typeDef.canonicalize(t), indices) match {
      case (PointerType(b,a), h::t) => PointerType(innerteof(b,t), a)
      case _ => throw new Exception("bad getelementptr " + t + " " + indices)
    }
  }


  def typeOf(sym:Value):Type = sym match {
    case x:Const => typeOf(x)
    case StructValue(fields, isPacked) => StructType(fields.map(typeOf(_)), isPacked)
    case ArrayValue(fields) => ArrayType(fields.length, typeOf(fields.head))
    case VectorValue(fields) => VectorType(fields.length, typeOf(fields.head))
    case x:VarOrID => toeTbl.getType(x).get
    case ArithmaticExpr(op, _, t, _, _) => t
    case BitwiseExpr(op, t, _, _) => t
    case CastExpr(_,_,t) => t
    case Call(_,_,retType,f,_,_) => retType
    case GetElemPtr(v, indices) => typeOf(typeOf(v), indices)
    case _ => throw new Exception("no type information for " + sym)
  }
}