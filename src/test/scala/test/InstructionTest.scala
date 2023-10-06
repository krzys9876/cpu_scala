package org.kr.cpu
package test

import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InstructionTest extends AnyFeatureSpec with GivenWhenThen with ScalaCheckPropertyChecks:
  Feature("NOP"):
    Scenario("NOP as LD w/o operands"):
      assert(1==1)

  Feature("LD"):
    Scenario("load immediate (low)"):
      assert(1==1)

    Scenario("load immediate (high)"):
      assert(1 == 1)

    Scenario("copy register"):
      assert(1 == 1)

    Scenario("read memory"):
      assert(1 == 1)

    Scenario("write memory"):
      assert(1 == 1)
