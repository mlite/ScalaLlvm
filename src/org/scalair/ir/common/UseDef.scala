package org.scalair.ir.common

/**
 * User: wangn
 * Date: 4/17/11
 */

trait Use {
  def uses:List[Use]
}

trait Def {
  def defined:Option[Use]
}

