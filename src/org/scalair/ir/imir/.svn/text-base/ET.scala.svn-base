package org.scalair.ir.imir

import org.scalair.hoopl.NonLocal
import org.scalair.hoopl.TypeDefines._

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 10/16/11
 * Time: 11:35 AM
 * To change this template use File | Settings | File Templates.
 */

case class ET[T](val env:ImEnv, v:T) extends NonLocal {
  def blockId():BlockId = {
    if (v.isInstanceOf[AbsNode])
      v.asInstanceOf[AbsNode].blockId
    else
      throw new Exception ()
  }
  def succBlockIds():List[BlockId] = {
    if (v.isInstanceOf[AbsNode])
      v.asInstanceOf[AbsNode].succBlockIds
    else
      throw new Exception ()
  }

  def asT = v
}

object ET {
  def asA[T](a:ET[_], v:T) = ET(a.env,v)
  def asT[T](a:ET[T]) = a.v
}