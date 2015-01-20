package org.scalair.ir.imir
/**
 * User: wangn
 * Date: 6/14/11
 */

abstract class ImEnv(val dataLayout:DataLayout) { self =>
  def localDef:ImEntityDefMap[Value]
  def labelDef:ImEntityDefMap[Int]
  def globalDef:ImEntityDefMap[Value]
  def typeDef:ImEntityDefMap[Type]
  def toeTbl:ImValueInfoMap

  def getCurOrUp(v:VarOrID):Type = toeTbl.getType(v) match {
    case Some(t0) => t0
    case None => throw new Exception()
  }

  def canonicalize(ty:Type):Type = {
    def f(fp:FormalParam) = FormalParam(canonicalize(fp.ty), fp.attr1, fp.align, None, fp.attr2)
    ty match {
      case TypeName(s) => typeDef.getDef(s) match {
        case Some(rhs) => rhs
        case None => throw new Exception("bad")
      }
      case TypeNo(n) => typeDef.getDef(n) match {
        case Some(rhs) => rhs
        case None => throw new Exception("bad")
      }
      case x:PrimitiveType => x
      case ArrayType(n, ety) => ArrayType(n, canonicalize(ety))
      case StructType(l, isPacked) => StructType(l.map(canonicalize(_)), isPacked)
      case VectorType(n, ety) => VectorType(n, canonicalize(ety))
      case PointerType(ety, addrSpace) => PointerType(canonicalize(ety), addrSpace)
      case FunType(retTy, fps, funAtts) =>
        FunType(canonicalize(retTy), FormalParamList(fps.list.map(f), fps.hasDots, fps.funAttrs), funAtts)
      case _ => throw new Exception()
    }
  }

  def typeOf(c:Const):Type = c match {
    case StructConst(fields, isPacked) => StructType(fields.map(typeOf(_)), isPacked)
    case ArrayConst(fields) => ArrayType(fields.length, typeOf(fields.head))
    case VectorConst(fields) => VectorType(fields.length, typeOf(fields.head))
    case PrimitiveConst(t, _) => t
    //case x:VarOrID => getCurOrUp(x)
    case ConstCast(_,_,t) => t
    case ConstArithmaticExpr(_,_,op1,op2) => typeOf(op1)
    case ConstFCmpExpr(_,_,_) => I(1)
    case ConstICmpExpr(_,_,_) => I(1)
    case ConstGetElemPtr(_, base, indices) => typeOf(typeOf(base), indices)
    case MDRef(n) => getCurOrUp(n)
    case MDConst(c) => MetadataType()
    case MDNode(n) => MetadataType()
    case MDString(n) => MetadataType()
    case MDVar(n) => MetadataType()
    case GlobalAddr(x) => PointerType(getCurOrUp(x))
    case _ => throw new Exception("typeOf " + c)
  }

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
    (canonicalize(t), indices) match {
      case (PointerType(b,a), h::t) => PointerType(innerteof(b,t), a)
      case _ => throw new Exception("bad getelementptr " + t + " " + indices)
    }
  }


  def typeOf(sym:Value):Type = sym match {
    case x:Const => typeOf(x)
    case StructValue(fields, isPacked) => StructType(fields.map(typeOf(_)), isPacked)
    case ArrayValue(fields) => ArrayType(fields.length, typeOf(fields.head))
    case VectorValue(fields) => VectorType(fields.length, typeOf(fields.head))
    case x:VarOrID => getCurOrUp(x)
    case ArithmaticExpr(op, _, t, _, _) => t
    case BitwiseExpr(op, t, _, _) => t
    case CastExpr(_,_,t) => t
    case Call(_,_,retType,f,_,_) => retType
    case GetElemPtr(v, indices) => typeOf(typeOf(v), indices)
    case ICmpExpr(_,_,_,_) => I(1)
    case FCmpExpr(_,_,_,_) => I(1)
    case Select(c,v,_) => typeOf(v)
    case _ => throw new Exception("no type information for " + sym)
  }

  def getLabelDef(s:String):Option[Int] = labelDef.getDef(s)
  def getLabelDef(s:Int):Option[Int] = labelDef.getDef(s)

  def addLocalDef(s:Int, rhs:Value) = new ImEnv(dataLayout) {
    def localDef = self.localDef.addDef(s, rhs)
    def labelDef = self.labelDef
    def globalDef = self.globalDef
    def typeDef = self.typeDef
    def toeTbl = self.toeTbl
  }

  def addLocalDef(s:String, rhs:Value) = new ImEnv(dataLayout) {
    def localDef = self.localDef.addDef(s, rhs)
    def labelDef = self.labelDef
    def globalDef = self.globalDef
    def typeDef = self.typeDef
    def toeTbl = self.toeTbl
  }

  def addGlobalDef(s:Int, rhs:Value) = new ImEnv(dataLayout) {
    def localDef = self.localDef
    def labelDef = self.labelDef
    def globalDef = self.globalDef.addDef(s,rhs)
    def typeDef = self.typeDef
    def toeTbl = self.toeTbl
  }

  def addGlobalDef(s:String, rhs:Value) = new ImEnv(dataLayout) {
    def localDef = self.localDef
    def labelDef = self.labelDef
    def globalDef = self.globalDef.addDef(s,rhs)
    def typeDef = self.typeDef
    def toeTbl = self.toeTbl
  }

  def addTypeDef(s:Int, rhs:Type) = new ImEnv(dataLayout) {
    def localDef = self.localDef
    def labelDef = self.labelDef
    def globalDef = self.globalDef
    def typeDef = self.typeDef.addDef(s, rhs)
    def toeTbl = self.toeTbl
  }

  def addTypeDef(s:String, rhs:Type) = new ImEnv(dataLayout) {
    def localDef = self.localDef
    def labelDef = self.labelDef
    def globalDef = self.globalDef
    def typeDef = self.typeDef.addDef(s, rhs)
    def toeTbl = self.toeTbl
  }

  def addLabelDef(s:String, rhs:Int) = new ImEnv(dataLayout) {
    def localDef = self.localDef
    def labelDef = self.labelDef.addDef(s, rhs)
    def globalDef = self.globalDef
    def typeDef = self.typeDef
    def toeTbl = self.toeTbl
  }

  def addLabelDef(s:Int, rhs:Int) = new ImEnv(dataLayout) {
    def localDef = self.localDef
    def labelDef = self.labelDef.addDef(s, rhs)
    def globalDef = self.globalDef
    def typeDef = self.typeDef
    def toeTbl = self.toeTbl
  }

  def add(v:Value, te:Type) = new ImEnv(dataLayout) {
    def localDef = self.localDef
    def labelDef = self.labelDef
    def globalDef = self.globalDef
    def typeDef = self.typeDef
    def toeTbl = self.toeTbl.add(v, te)
  }

  def add(v:Option[Value], te:Type) = new ImEnv(dataLayout) {
    def localDef = self.localDef
    def labelDef = self.labelDef
    def globalDef = self.globalDef
    def typeDef = self.typeDef
    def toeTbl = self.toeTbl.add(v, te)
  }

  private def insert[T <: Value](v:T, te:Type):(T, ImEnv) = {
    val ns = new ImEnv(dataLayout) {
      def localDef = self.localDef
      def labelDef = self.labelDef
      def globalDef = self.globalDef
      def typeDef = self.typeDef
      def toeTbl = self.toeTbl.add(v, te)
    }
    (v, ns)
  }


  def getNewGlobalID(te:Type, rhs:Option[Value]=None):(GlobalID, ImEnv)  = {
    val rhs0 = rhs.getOrElse(PrimitiveConst(te, Undef()))
    val (lhs, globalDef0) = globalDef.getNewID(rhs0)
    val ns = new ImEnv(dataLayout) {
      def localDef: ImEntityDefMap[Value] = self.localDef
      def toeTbl: ImValueInfoMap = self.toeTbl
      def labelDef: ImEntityDefMap[Int] = self.labelDef
      def typeDef: ImEntityDefMap[Type] = self.typeDef
      def globalDef: ImEntityDefMap[Value] = globalDef0
    }
    ns.insert(GlobalID(lhs), te)
  }
  def getNewGlobalVar(s:String, te:Type, rhs:Option[Value]=None):(GlobalVar, ImEnv) = {
    val rhs0 = rhs.getOrElse(PrimitiveConst(te, Undef()))
    val (lhs, globalDef0) = globalDef.getNewName(s, rhs0)
    val ns = new ImEnv(dataLayout) {
      def localDef: ImEntityDefMap[Value] = self.localDef
      def toeTbl: ImValueInfoMap = self.toeTbl
      def labelDef: ImEntityDefMap[Int] = self.labelDef
      def typeDef: ImEntityDefMap[Type] = self.typeDef
      def globalDef: ImEntityDefMap[Value] = globalDef0
    }
    ns.insert(GlobalVar(lhs), te)
  }

  def getNewLocalID(te:Type, rhs:Option[Value]=None):(LocalID, ImEnv) = {
    val rhs0 = rhs.getOrElse(PrimitiveConst(te, Undef()))
    val (lhs, localDef0) = localDef.getNewID(rhs0)
    val ns = new ImEnv(dataLayout) {
      def localDef: ImEntityDefMap[Value] = localDef0
      def toeTbl: ImValueInfoMap = self.toeTbl
      def labelDef: ImEntityDefMap[Int] = self.labelDef
      def typeDef: ImEntityDefMap[Type] = self.typeDef
      def globalDef: ImEntityDefMap[Value] = self.globalDef
    }
    ns.insert(LocalID(lhs), te)
  }

  def getNewLocalVar(s:String, te:Type, rhs:Option[Value]=None):(LocalVar, ImEnv) = {
    val rhs0 = rhs.getOrElse(PrimitiveConst(te, Undef()))
    val (lhs, localDef0) = localDef.getNewName(s, rhs0)
    val ns = new ImEnv(dataLayout) {
      def localDef: ImEntityDefMap[Value] = localDef0
      def toeTbl: ImValueInfoMap = self.toeTbl
      def labelDef: ImEntityDefMap[Int] = self.labelDef
      def typeDef: ImEntityDefMap[Type] = self.typeDef
      def globalDef: ImEntityDefMap[Value] = self.globalDef
    }
    ns.insert(LocalVar(lhs), te)
  }

  def sizeOfInBit(ty:Type):Int = ty match {
    case x:PrimitiveType => x.bits
    case x:PointerType => dataLayout.ptrSize
    case _ => throw new Exception("sizeofInBit " + ty)
  }

  def sizeofInByte(ty:Type):Int = ty match {
    case x:PrimitiveType => x.bits / 8
    case x:PointerType => dataLayout.ptrSize / 8
    case _ => throw new Exception ("sizeofInByte " + ty)
  }
  def sizeOfInByte(ty:Type):PrimitiveConst = PrimitiveConst(I(32), SizeOf(ty))
}

object ImEnv {
  def functionScope(d:DataLayout) = new ImEnv(d) {
    val localDef:ImEntityDefMap[Value] = new ImEntityDefMap[Value]()
    val labelDef:ImEntityDefMap[Int] = new ImEntityDefMap[Int]()
    val globalDef:ImEntityDefMap[Value] = new ImEntityDefMap[Value]()
    val typeDef:ImEntityDefMap[Type] = new ImEntityDefMap[Type]()
    val toeTbl:ImValueInfoMap = new ImValueInfoMap()
  }
}