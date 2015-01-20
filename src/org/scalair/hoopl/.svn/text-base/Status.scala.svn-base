package org.scalair.hoopl

import TypeDefines._

/**
 * User: wangn
 * Date: 6/17/11
 */
sealed abstract class Status()
final case class O() extends Status()
final case class C() extends Status()


trait Dot {
  def escape(s:String):String = s
  def toDotLabelRow:String
  def toDotLabelTable:String = makeTable(toDotLabelRow)
  def colorFont(s:String, color:String) = "<font color=\"" + color + "\">" +s+ "</font>"
  def makeLeftRow(s:String, bgcolor:String) = "<tr><td align=\"left\" bgcolor=\"" + bgcolor + "\">" + s + "</td></tr>"
  def makeTable(s:String) = "<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\">" + s + "</table>>"

  def quote(s:String) = "\"" + s + "\""

  def makeNode(n:String, label:String) =
    quote(n) + "[" +
      " style = " + quote("filled, bold") +
      " penwidth = 1" +
      " fillcolor = " + quote("white") +
      //" shape = "  + quote("Mrecord") +
      " label = " + label + "]"
}
trait NonLocal {
  def blockId():BlockId
  def succBlockIds():List[BlockId]
}

trait LabelsPtr {
  def targetBlockIds():List[BlockId]
}
trait EXShape[E<:Status, X<:Status] {
  type EXShapeImpl[E1<:Status, X1<:Status] <: EXShape[E1,X1]

  val e:E
  val x:X
  def castEX[E1<:Status,X1<:Status] = {
    if (e.isInstanceOf[E1] && x.isInstanceOf[X1])
      this.asInstanceOf[EXShapeImpl[E1,X1]]
    else
      throw new Exception()
  }
}

trait COShape extends EXShape[C,O] {
  val e = C()
  val x = O()
}
trait OOShape extends EXShape[O,O] {
  val e = O()
  val x = O()
}
trait OCShape extends EXShape[O,C] {
  val e = O()
  val x = C()
}
trait CCShape extends EXShape[C,C] {
  val e = C()
  val x = C()
}


sealed abstract class MaybeO[EX<:Status,T](val status:EX) {
  def map[T1](f:T=>T1):MaybeO[EX, T1]
  def get:T
}
final case class JustO[T](t:T) extends MaybeO[O,T](O()) {
  def map[T1](f:T=>T1):JustO[T1] = JustO[T1](f(t))
  def get = t
}
final case class NothingO[T]() extends MaybeO[C,T](C()) {
  def map[T1](f:T=>T1) = NothingO[T1]()
  def get = throw new Exception()
}

sealed abstract class MaybeC[EX <:Status,T](val status:EX) {
  def map[T1](f:T=>T1):MaybeC[EX, T1]
  def get:T
}
final case class JustC[T](t:T) extends MaybeC[C,T](C()) {
  def map[T1](f:T=>T1):JustC[T1] = JustC[T1](f(t))
  def get = t
}
final case class NothingC[T]() extends MaybeC[O,T](O()) {
  def map[T1](f:T=>T1):NothingC[T1] = NothingC[T1]()
  def get = throw new Exception()
}



/*
abstract class Shape(ex:Status)
case object Closed extends Shape(C())
case object Open extends Shape(O())

object IndexedCO {
  def select[A,B](s:Status, a:A, b:B) = s match {
    case O => a
    case C => b
  }
}
*/