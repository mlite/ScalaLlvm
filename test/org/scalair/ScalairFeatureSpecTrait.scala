package org.scalair

import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import org.scalatest.{GivenWhenThen, FeatureSpec}

/**
 * User: wangn
 * Date: 10/2/11
 */

trait ScalairFeatureSpecTrait extends ScalairTestTrait
with FeatureSpec with MustMatchers with ShouldMatchers with GivenWhenThen {
}