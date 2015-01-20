package org.scalair.ir.imir

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 5/23/11
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */
object ImEntityDefMap {
  private var count:Int = 0;

  final def getNextNum:Int = {
    val ret = count
    count = count + 1
    ret
  }
}


case class ImEntityDefMap[T](val named:Map[String, T] = Map[String,T](), val unnamed:Map[Int, T] = Map[Int, T]()) {
  import ImEntityDefMap.getNextNum

  type ImplMap = ImEntityDefMap[T]
  def this(e:ImEntityDefMap[T]) = this(e.named, e.unnamed)
  def newMap(m1:Map[String,T], m2:Map[Int, T]):ImplMap = ImEntityDefMap(m1, m2)

  def getNewID(t:T):(Int, ImplMap) = {
    val n = getNextNum
    if (unnamed.get(n).isDefined) {
      getNewID(t)
    } else {
      (n, defUnnamed(n, t))
    }
  }

  def getNewName(s:String, t:T):(String, ImplMap) = {
    val n = s + getNextNum
    if (named.get(n).isDefined) {
      getNewName(s, t)
    } else {
      (n, defNamed(n, t))
    }
  }

  private def defNamed(s:String, t:T) = newMap(named + (s -> t), unnamed)
  private def defUnnamed(n:Int, t:T) = newMap(named, unnamed + (n -> t))

  def addDef(s:String, t:T) = {
    named.get(s) match {
      case Some(rhs) => throw new Exception (s + " is previously defined as " + rhs)
      case None => newMap (named + (s -> t), unnamed)
    }
  }

  def addDef(s:Int, t:T):ImplMap = {
    unnamed.get(s) match {
      case Some(rhs) => throw new Exception (s + " is previously defined as " + rhs)
      case None => newMap (named, unnamed + (s -> t))
    }
  }

  def getDef(s:String):Option[T] = named.get(s)
  def getDef(n:Int):Option[T] = unnamed.get(n)
}