package org.scalair.hoopl

/**
 * User: wangn
 * Date: 7/23/11
 */

object Pointed {
  def gettop[A<:Ordered[A]]():Pointed[A] = new Pointed[A](C(),O(), None) {
    def map[A1<:Ordered[A1]](f:A=>A1) = gettop[A1]()
    def get = throw new NoSuchElementException("top.data")
    def getOrElse(a:A) = a
  }
  def getbot[A<:Ordered[A]]():Pointed[A] = new Pointed[A](O(), C(), None) {
    def map[A1<:Ordered[A1]](f:A=>A1) = getbot[A1]()
    def get = throw new NoSuchElementException("bot.data")
    def getOrElse(a:A) = a
  }
  def pelem[A<:Ordered[A]](a:A):Pointed[A] = {
    new Pointed[A](O(),O(),Some(a)) {
      def map[A1<:Ordered[A1]](f:A=>A1) = pelem[A1](f(a))
      def get = data.get
      def getOrElse(a:A) = get
    }
  }
  def withBot[A<:Ordered[A]](a:A):Pointed[A] = {
    new Pointed[A](O(), C(), Some(a)) {
      def map[A1<:Ordered[A1]](f:A=>A1) = withBot[A1](f(a))
      def get = data.get
      def getOrElse(a:A) = get
    }
  }
  def withTop[A<:Ordered[A]](a:A):Pointed[A] = {
    new Pointed[A](C(), O(), Some(a)) {
      def map[A1<:Ordered[A1]](f:A=>A1) = withTop[A1](f(a))
      def get = data.get
      def getOrElse(a:A) = get
    }
  }
  def withTopAndBot[A<:Ordered[A]](a:A):Pointed[A] = {
    new Pointed[A](C(), C(), Some(a)) {
      def map[A1<:Ordered[A1]](f:A=>A1) = withTopAndBot[A1](f(a))
      def get = data.get
      def getOrElse(a:A) = get
    }
  }
}

/*
 *  Top = Pointed(C(),_,None())
 *  Bot = Pointed(_,C(),None())
 *  PElem[a] = Pointed(_,_,Some(a))
 */

abstract case class Pointed[A<:Ordered[A]](val top:Status, val bot:Status, val data:Option[A])
  extends Ordered[Pointed[A]] {
  def map[A1<:Ordered[A1]](f:A=>A1):Pointed[A1]
  def compare(that:Pointed[A]):Int = {
    (this, that) match  {
      case (Pointed(_,C(),None), Pointed(_,C(),None)) => 0
      case (Pointed(_,C(),None), _) => -1
      case (_, Pointed(_,C(),None)) => 1
      case (Pointed(_,_,Some(a1)), Pointed(_,_,Some(a2))) => a1.compare(a2)
      case (Pointed(C(),_,None), Pointed(C(),_,None)) => 0
      case (Pointed(C(),_,None), _) => 1
      case (_, Pointed(C(),_,None)) => -1
    }
  }
  def get:A
  def getOrElse(a:A):A
  def getOrNone:Option[A] = data
  override def toString = {
    (top, bot, data) match {
      case (C(),_,None) => "T"
      case (_,C(),None) => "_|_"
      case (_,_,Some(a)) => a.toString
    }
  }
}