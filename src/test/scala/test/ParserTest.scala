package org.kr.cpu
package test

import org.scalactic.anyvals.PosZInt
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParserTest extends AnyFeatureSpec with GivenWhenThen with ScalaCheckPropertyChecks:
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50, maxDiscardedFactor = 30.0, minSize = PosZInt(100))

  Feature("Parse single instructions"):
    Scenario("parse LD RR"):
      forAll(TestUtils.registerIndexGen, TestUtils.registerIndexGen):
        (r1, r2) =>
          assert(LineParser().process(f"LD R$r1%01X R$r2%01X").contains(INSTR_LD_RR(r1,r2)))
          assert(LineParser().process(f"LDZ R$r1%01X R$r2%01X").contains(INSTR_LDZ_RR(r1, r2)))
          assert(LineParser().process(f"LDNZ R$r1%01X R$r2%01X").contains(INSTR_LDNZ_RR(r1, r2)))

    Scenario("parse LD MR"):
      forAll(TestUtils.registerIndexGen, TestUtils.registerIndexGen):
        (r1, r2) =>
          assert(LineParser().process(f"LD M$r1%01X R$r2%01X").contains(INSTR_LD_MR(r1, r2)))
          assert(LineParser().process(f"LDZ M$r1%01X R$r2%01X").contains(INSTR_LDZ_MR(r1, r2)))
          assert(LineParser().process(f"LDNZ M$r1%01X R$r2%01X").contains(INSTR_LDNZ_MR(r1, r2)))

    Scenario("parse LD RM"):
      forAll(TestUtils.registerIndexGen, TestUtils.registerIndexGen):
        (r1, r2) =>
          assert(LineParser().process(f"LD R$r1%01X M$r2%01X").contains(INSTR_LD_RM(r1, r2)))
          assert(LineParser().process(f"LDZ R$r1%01X M$r2%01X").contains(INSTR_LDZ_RM(r1, r2)))
          assert(LineParser().process(f"LDNZ R$r1%01X M$r2%01X").contains(INSTR_LDNZ_RM(r1, r2)))

    Scenario("parse JMP R (JP Rx 0)"):
      forAll(TestUtils.registerIndexGen):
        r =>
          assert(LineParser().process(f"JMP R$r%01X").contains(INSTR_LD_RR(r, 0)))
          assert(LineParser().process(f"JMPZ R$r%01X").contains(INSTR_LDZ_RR(r, 0)))
          assert(LineParser().process(f"JMPNZ R$r%01X").contains(INSTR_LDNZ_RR(r, 0)))

    Scenario("parse JMP M (JP (Rx) 0)"):
      forAll(TestUtils.registerIndexGen):
        r =>
          assert(LineParser().process(f"JMP M$r%01X").contains(INSTR_LD_MR(r, 0)))
          assert(LineParser().process(f"JMPZ M$r%01X").contains(INSTR_LDZ_MR(r, 0)))
          assert(LineParser().process(f"JMPNZ M$r%01X").contains(INSTR_LDNZ_MR(r, 0)))

    Scenario("parse IN R P"):
      forAll(TestUtils.registerIndexGen, TestUtils.portGen):
        (r, p) =>
          assert(LineParser().process(f"IN R$r%01X P$p%01X").contains(INSTR_LD_RI(r, p)))
          assert(LineParser().process(f"INZ R$r%01X P$p%01X").contains(INSTR_LDZ_RI(r, p)))
          assert(LineParser().process(f"INNZ R$r%01X P$p%01X").contains(INSTR_LDNZ_RI(r, p)))

    Scenario("parse OUT R P"):
      forAll(TestUtils.registerIndexGen, TestUtils.portGen):
        (r, p) =>
          assert(LineParser().process(f"OUT R$r%01X P$p%01X").contains(INSTR_LD_RO(r, p)))
          assert(LineParser().process(f"OUTZ R$r%01X P$p%01X").contains(INSTR_LDZ_RO(r, p)))
          assert(LineParser().process(f"OUTNZ R$r%01X P$p%01X").contains(INSTR_LDNZ_RO(r, p)))
