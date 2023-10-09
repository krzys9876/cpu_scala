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
      val cpuStart = TestUtils.createRandomStateCpu
      forAll(TestUtils.addressGen):
        pc =>
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, INSTR_NOP.value)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("PC is increased by 1")
          //println(f"PC before: ${cpuNop.pc}%04X PC after: ${cpuAfter.pc}%04X")
          assert(cpuAfter.pc == (pc + 1).toShort)
          And("no other change is made")
          assert((1 to 15).forall(r => cpuAfter.register(r) == cpuBefore.register(r)))

  Feature("LD"):
    Scenario("load immediate (low)"):
      Given("a CPU in random state with LD AL,X as next instruction")
      val cpuStart = TestUtils.createRandomStateCpu
      forAll(TestUtils.addressGen,TestUtils.byteValueGen):
        (pc,imm) =>
          val aBefore=cpuStart.a
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, INSTR_LD_AL(imm).value)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("A (low) is loaded with operand")
          assert(cpuAfter.a == ((aBefore & 0xFF00) | imm).toShort)
          And("PC is increased by 1")
          assert(cpuAfter.pc == (pc + 1).toShort)

    Scenario("load immediate (high)"):
      Given("a CPU in random state with LD AH,X as next instruction")
      val cpuStart = TestUtils.createRandomStateCpu
      forAll(TestUtils.addressGen, TestUtils.byteValueGen):
        (pc, imm) =>
          val aBefore = cpuStart.a
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, INSTR_LD_AH(imm).value)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("A (high) is loaded with operand")
          assert(cpuAfter.a == ((aBefore & 0x00FF) | (imm << 8)).toShort)
          And("PC is increased by 1")
          assert(cpuAfter.pc == (pc + 1).toShort)

    Scenario("copy register"):
      assert(1 == 1)

    Scenario("read memory"):
      assert(1 == 1)

    Scenario("write memory"):
      assert(1 == 1)
