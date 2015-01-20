package org.scalair.ir.imir

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 5/7/11
 * Time: 11:50 PM
 * To change this template use File | Settings | File Templates.
 */

case class ValueInfo(val qname:String, val te:Type)

case class ValueInfoMap(private var map:Map[Value, ValueInfo] = Map()) {
  //import typeEntityDefMap.canonicalize;

  def this(t:ValueInfoMap) = this(t.map)
  //def addDef(s:String, typ:Type) = typeEntityDefMap.addDef(s, typ)
  //def addDef(s:Int, typ:Type) = typeEntityDefMap.addDef(s, typ)

  def add(sym:Value, typ:Type) =
    map += (sym -> ValueInfo(sym.toString, typ))

  def add(sym:Option[Value], typ:Type) = sym match {
    case Some(s) => map += (s -> ValueInfo(s.toString, typ))
    case None => ()
  }

  def getType(sym:Value):Option[Type] = map.get(sym) match {
    case Some(vi) => Some(vi.te)
    case None => None
  }
}



case class ImValueInfoMap(val map:Map[Value, ValueInfo] = Map()) {
  def this(t:ImValueInfoMap) = this(t.map)

  def add(sym:Value, typ:Type) = ImValueInfoMap(map + (sym -> ValueInfo(sym.toString, typ)))

  def add(sym:Option[Value], typ:Type) = sym match {
    case Some(s) => ImValueInfoMap(map + (s -> ValueInfo(s.toString, typ)))
    case None => this
  }

  def getType(sym:Value):Option[Type] = map.get(sym) match {
    case Some(vi) => Some(vi.te)
    case None => None
  }
}


// Target Type Entity (TTE => Tte)
