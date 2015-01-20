package org.scalair.hoopl.monad

/**
 * User: wangn
 * Date: 9/28/11
 */

class LazyList(private val hd:Int, private val upbound:Option[Int]) {
  def toList:List[Int] = upbound match {
    case Some(up) => hd.until(up).toList
    case None => throw new Exception ("cannot convert infinite lazy list to a list")
  }
  def head = hd
  def tail = this match {
    case LazyList(h, t) => t
    case _ => throw new Exception()
  }

  override def toString = "LazyList(hd: " + hd + ", upbound: " + upbound + ")"
}

object LazyList {
  def apply(hd:Int) = new LazyList(hd, None)
  def apply(hd:Int, upbound:Int) = new LazyList(hd, Some(upbound))
  def unapply(ll:LazyList):Option[(Int, LazyList)] = {
    ll.upbound match {
      case Some(up) => {
        if (ll.hd < up) Some((ll.hd, new LazyList(ll.hd+1, ll.upbound)))
        else None
      }
      case None => Some((ll.hd, new LazyList(ll.hd+1, ll.upbound)))
    }
  }



  def print(l:LazyList) {
    l match {
      case LazyList(hd, tail) => {
        Console.println(hd)
        print(tail)
      }
      case _ => Console.println("end")
    }
  }
}
