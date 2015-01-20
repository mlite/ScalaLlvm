package org.scalair.util

/**
 * Created by IntelliJ IDEA.
 * User: wangn
 * Date: 10/7/11
 * Time: 10:48 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory

trait Logging {
  lazy val logger = LoggerFactory.getLogger(getClass)
}