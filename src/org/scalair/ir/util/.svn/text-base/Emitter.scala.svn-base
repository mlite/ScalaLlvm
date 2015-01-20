package org.scalair.ir.util

import java.io.PrintWriter

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 4/17/11
 * Time: 8:14 PM
 * To change this template use File | Settings | Filels
 * Templates.
 */

trait Emitter {
  def emit(out:PrintWriter, indent:String) {
    out.print(indent); out.print(toString)
  }

  def optToString(before:String, e:Option[Emitter]):String =
    if (e.isDefined) before + e.get.toString
    else ""

  def optToString(e:Option[Emitter]):String =
    if (e.isDefined) e.get.toString
    else ""

  def optToString(e:Option[Emitter], after:String):String =
    if (e.isDefined) e.get.toString + after
    else ""

  def listToString(before:String, l:List[Any], sep:String, after:String):String =
    (if (l.isEmpty) "" else before + (l mkString sep)) + after
}

