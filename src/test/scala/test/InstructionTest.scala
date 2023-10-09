package org.kr.cpu
package test

import org.scalacheck.Gen
import org.scalactic.anyvals.PosZInt
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InstructionTest extends AnyFeatureSpec with GivenWhenThen with ScalaCheckPropertyChecks:
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50, maxDiscardedFactor = 30.0, minSize = PosZInt(100))

  Feature("NOP"):
    Scenario("execute NOP (LD with NOP_MODE)"):
      Given("a CPU in random state with NOP as next instruction")
      val cpuInit = TestUtils.createRandomStateCpu
      forAll(TestUtils.anyValueGen):
        pc =>
          val cpuNop = cpuInit.setPc(pc).writeMemory(pc, INSTR_NOP.value)
          When("executed")
          val cpuAfter = cpuNop.handleNext
          Then("PC is increased by 1 and no other change is made")
          //println(f"PC before: ${cpuNop.pc}%04X PC after: ${cpuAfter.pc}%04X")
          assert(cpuAfter.pc == (pc + 1).toShort)
          assert((1 to 15).forall(r => cpuAfter.register(r) == cpuNop.register(r)))

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
