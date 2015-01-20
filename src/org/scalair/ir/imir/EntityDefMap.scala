package org.scalair.ir.imir

/**
 * User: wangn
 * Date: 5/23/11
 */

class EntityDefMap[T](private var named:Map[String, T], private var unnamed:Map[Int, T]) {
  private[this] var unnamedCount = 0
  private[this] var namedCount = 0

  def this() = this(Map[String, T](), Map[Int, T]())
  def this(e:EntityDefMap[T]) = this(e.named, e.unnamed)

  def getNewID(t:T):Int = {
    val n = unnamedCount
    if (unnamed.get(n).isDefined) {
      unnamedCount += 1
      getNewID(t)
    } else {
      defUnnamed(n, t)
      n
    }
  }

  def getNewName(s:String, t:T):String = {
    val n = s + namedCount
    if (named.get(n).isDefined) {
      namedCount += 1
      getNewName(s, t)
    } else {
      defNamed(n, t)
      n
    }
  }

  private def defNamed(s:String, t:T) {
    named += (s -> t)
    namedCount += 1
  }

  private def defUnnamed(n:Int, t:T) {
    unnamed += (n -> t)
    unnamedCount += 1
  }

  def addDef(s:String, t:T) {
    named.get(s) match {
      case Some(rhs) => throw new Exception (s + " is previously defined as " + rhs)
      case None => named += (s -> t)
    }
  }

  def addDef(s:Int, t:T) {
    unnamed.get(s) match {
      case Some(rhs) => throw new Exception (s + " is previously defined as " + rhs)
      case None => unnamed += (s -> t)
    }
  }

  def getDef(s:String):Option[T] = named.get(s)
  def getDef(n:Int):Option[T] = unnamed.get(n)
}