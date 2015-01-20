package org.scalair.ir.imir

/**
 * User: wangn
 * Date: 5/12/11
 */

case class CommonExpCache[E, T]() {
  private var disable_ = false
  private var map:Map[E, T] = Map()
  def add(e:E, v:T) {
    if (!disable_) map += (e -> v)
  }
  def get(e:E):Option[T] = map.get(e)
  def disable = disable_ = true
}