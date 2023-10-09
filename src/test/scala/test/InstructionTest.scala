package org.kr.cpu
package test

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

    Scenario("copy between registers"):
      Given("a CPU in random state with LD r1,r2 as next instruction")
      And("r1 and r2 are not PC")
      val cpuStart = TestUtils.createRandomStateCpu
      forAll(TestUtils.addressGen, TestUtils.registerIndexGen, TestUtils.registerIndexGen):
        (pc, r1, r2) => whenever(r1!=0 && r2!=0):
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, INSTR_LD_RR(r1,r2).value)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("register 2 is loaded with value from register 1")
          //println(f"R1($r1): ${cpuAfter.register(r1)} R2($r2) after: ${cpuAfter.register(r2)} R2 before: ${cpuBefore.register(r2)}")
          assert(cpuAfter.register(r2) == cpuAfter.register(r1))
          And("PC is increased by 1")
          assert(cpuAfter.pc == (pc + 1).toShort)

    Scenario("copy from PC to a register"):
      Given("a CPU in random state with LD r1,r2 as next instruction")
      And("r1 is PC and r2 is not PC")
      val cpuStart = TestUtils.createRandomStateCpu
      forAll(TestUtils.addressGen, TestUtils.registerIndexGen):
        (pc, r2) => whenever(r2 != 0):
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, INSTR_LD_RR(0, r2).value)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("register 2 is loaded with value from PC (before increase)")
          //println(f"R1(0): ${cpuAfter.pc} R2($r2) after: ${cpuAfter.register(r2)} R2 before: ${cpuBefore.register(r2)}")
          assert(cpuAfter.register(r2) == pc.toShort)
          And("PC is increased by 1")
          assert(cpuAfter.pc == (pc + 1).toShort)

    Scenario("copy from register to PC (jump)"):
      Given("a CPU in random state with LD r1,r2 as next instruction")
      And("r2 is PC and r1 is not PC")
      val cpuStart = TestUtils.createRandomStateCpu
      forAll(TestUtils.addressGen, TestUtils.registerIndexGen):
        (pc, r1) => whenever(r1 != 0):
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, INSTR_LD_RR(r1, 0).value)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("PC is loaded with value from register 1")
          //println(f"pc: $pc R1($r1): ${cpuAfter.register(r1)} R2(0) after: ${cpuAfter.pc} R2 before: ${cpuBefore.pc}")
          assert(cpuAfter.pc == cpuAfter.register(r1))

    Scenario("read memory"):
      Given("a CPU in random state with LD (r1),r2 as next instruction")
      And("r1 and r2 are not PC")
      val cpuStart = TestUtils.createRandomStateCpuWithMemory()
      forAll(TestUtils.addressGen, TestUtils.registerIndexGen, TestUtils.registerIndexGen):
        (pc, r1, r2) =>
          whenever(r1 != 0 && r2 != 0 && cpuStart.memory(cpuStart.register(r1))!=0):
            val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, INSTR_LD_MR(r1, r2).value)
            When("executed")
            val sourceMemory = cpuBefore.memory(cpuBefore.register(r1)) // this is to copy value in case both registers are equal
            val cpuAfter = cpuBefore.handleNext
            Then("register 2 is loaded with memory contents at address from register 1")
            println(f"(R1)($r1): ${cpuAfter.register(r1)} ${cpuAfter.memory(cpuAfter.register(r1))} R2($r2) after: ${cpuAfter.register(r2)} R2 before: ${cpuBefore.register(r2)}")
            assert(cpuAfter.register(r2) == sourceMemory)
            And("PC is increased by 1")
            assert(cpuAfter.pc == (pc + 1).toShort)

    Scenario("write memory"):
      Given("a CPU in random state with LD r1,(r2) as next instruction")
      And("r1 and r2 are not PC")
      val cpuStart = TestUtils.createRandomStateCpuWithMemory()
      forAll(TestUtils.addressGen, TestUtils.registerIndexGen, TestUtils.registerIndexGen):
        (pc, r1, r2) =>
          whenever(r1 != 0 && r2 != 0 && cpuStart.memory(cpuStart.register(r1)) != 0):
            val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, INSTR_LD_RM(r1, r2).value)
            When("executed")
            val cpuAfter = cpuBefore.handleNext
            Then("memory at address from register 2 is loaded with register 1")
            println(f"(R1)($r1): ${cpuAfter.register(r1)} R2($r2) after: ${cpuAfter.register(r2)} ${cpuAfter.memory(cpuAfter.register(r2))}  R2 before: ${cpuBefore.memory(cpuBefore.register(r2))}")
            assert(cpuAfter.memory(cpuAfter.register(r2)) == cpuAfter.register(r1))
            And("PC is increased by 1")
            assert(cpuAfter.pc == (pc + 1).toShort)
