package org.kr.cpu
package test

import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ParserTest extends AnyFeatureSpec with GivenWhenThen:
  Feature("Parse single instructions"):
    Scenario("parse LD RR"):
      assert(LineParser().process("LD R0 RB").contains(INSTR_LD_RR(0,0xB)))

    Scenario("parse LD MR"):
      assert(LineParser().process("LD M2 RB").contains(INSTR_LD_MR(2, 0xB)))

