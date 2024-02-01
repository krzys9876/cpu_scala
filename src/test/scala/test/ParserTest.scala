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

    Scenario("parse LD RM"):
      assert(LineParser().process("LD R2 M7").contains(INSTR_LD_RM(2, 7)))

    Scenario("parse JMP R"):
      assert(LineParser().process("JMP R7").contains(INSTR_LD_RR(7, 0)))

    Scenario("parse IN R P"):
      assert(LineParser().process("IN R7 PA").contains(INSTR_LD_RI(7, 0xA)))

    Scenario("parse OUT R P"):
      assert(LineParser().process("OUT RC P2").contains(INSTR_LD_RO(0xC, 2)))
