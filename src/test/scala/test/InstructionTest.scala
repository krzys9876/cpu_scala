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
      doTestNOP(TestUtils.createRandomStateCpu,INSTR_NOP())

  Feature("LD"):
    Scenario("load immediate (low)"):
      Given("a CPU in random state with LD AL,X as next instruction")
      doTestLDx_AL(TestUtils.createRandomStateCpu,INSTR_LD_AL.apply)

    Scenario("load immediate (high)"):
      Given("a CPU in random state with LD AH,X as next instruction")
      doTestLDx_AH(TestUtils.createRandomStateCpu,INSTR_LD_AH.apply)

    Scenario("copy between registers"):
      Given("a CPU in random state with LD r1,r2 as next instruction")
      And("r1 and r2 are not PC")
      doTestLDx_RR(TestUtils.createRandomStateCpu,INSTR_LD_RR.apply, _ != 0 && _ != 0)

    Scenario("copy from PC to a register"):
      Given("a CPU in random state with LD r1,r2 as next instruction")
      And("r2 is PC")
      doTestLDx_PC_R(TestUtils.createRandomStateCpu,INSTR_LD_RR(0,_), _ != 0)

    Scenario("copy from register to PC (jump unconditional)"):
      Given("a CPU in random state with LD r1,r2 as next instruction")
      And("r2 is PC and r1 is not PC")
      doTestLDx_R_PC(TestUtils.createRandomStateCpu,INSTR_LD_RR(_, 0), _ != 0)

    Scenario("read memory to register"):
      Given("a CPU in random state with LD (r1),r2 as next instruction")
      And("r2 is not PC")
      doTestLDx_MR(TestUtils.createRandomStateCpuWithMemory(), INSTR_LD_MR.apply, _ != 0)

    Scenario("read memory to PC (jump)"):
      Given("a CPU in random state with LD (r1),r2 as next instruction")
      And("r2 is PC")
      doTestLDx_M_PC(TestUtils.createRandomStateCpuWithMemory(),INSTR_LD_MR(_, 0))

    Scenario("write memory"):
      Given("a CPU in random state with LD r1,(r2) as next instruction")
      doTestLDx_RM(TestUtils.createRandomStateCpuWithMemory(),INSTR_LD_RM.apply)

  Feature("LDZ/LDNZ"):
    Scenario("load immediate (low) when Z flag matches instruction condition"):
      Given("a CPU in random state with LD(N)Z AL,X as next instruction")
      doTestLDx_AL(TestUtils.createRandomStateCpu.setZ(),INSTR_LDZ_AL.apply)
      doTestLDx_AL(TestUtils.createRandomStateCpu.clearZ(),INSTR_LDNZ_AL.apply)

    Scenario("load immediate (low) when Z flag does not match instruction condition"):
      Given("a CPU in random state with LD(N)Z AL,X as next instruction")
      doTestNOP(TestUtils.createRandomStateCpu.clearZ(), INSTR_LDZ_AL(0x1234))
      doTestNOP(TestUtils.createRandomStateCpu.setZ(), INSTR_LDNZ_AL(0x1234))

    Scenario("load immediate (high) when Z flag matches instruction condition"):
      Given("a CPU in random state with LD(N)Z AH,X as next instruction")
      doTestLDx_AH(TestUtils.createRandomStateCpu.setZ(), INSTR_LDZ_AH.apply)
      doTestLDx_AH(TestUtils.createRandomStateCpu.clearZ(), INSTR_LDNZ_AH.apply)

    Scenario("load immediate (high) when Z flag does not match instruction condition"):
      Given("a CPU in random state with LD(N)Z AH,X as next instruction")
      doTestNOP(TestUtils.createRandomStateCpu.clearZ(), INSTR_LDZ_AH(0x1234))
      doTestNOP(TestUtils.createRandomStateCpu.setZ(), INSTR_LDNZ_AH(0x1234))

    Scenario("copy registers when Z flag matches instruction condition"):
      Given("a CPU in random state with LD(N)Z r1,r2 as next instruction")
      And("r1 and r2 are not PC")
      doTestLDx_RR(TestUtils.createRandomStateCpu.setZ(), INSTR_LDZ_RR.apply, (r1,r2) => r1!= 0 && r2!= 0 && r1!=2 && r2!=2)
      doTestLDx_RR(TestUtils.createRandomStateCpu.clearZ(), INSTR_LDNZ_RR.apply, (r1,r2) => r1!= 0 && r2!= 0 && r1!=2 && r2!=2)

    Scenario("copy registers when Z flag does not match instruction condition"):
      Given("a CPU in random state with LD(N)Z r1,r2 as next instruction")
      And("r1 and r2 are not PC")
      doTestNOP(TestUtils.createRandomStateCpu.clearZ(), INSTR_LDZ_RR(0xA,0xB))
      doTestNOP(TestUtils.createRandomStateCpu.setZ(), INSTR_LDNZ_RR(0xC,0xD))

    Scenario("copy from PC to a register when Z flag matches instruction condition"):
      Given("a CPU in random state with LD(N)Z r1,r2 as next instruction")
      And("r2 is PC")
      doTestLDx_PC_R(TestUtils.createRandomStateCpu.setZ(), INSTR_LDZ_RR(0, _), r => r != 0 && r != 2)
      doTestLDx_PC_R(TestUtils.createRandomStateCpu.clearZ(), INSTR_LDNZ_RR(0, _), r => r != 0 && r != 2)

    Scenario("copy from PC to a register when Z flag does not match instruction condition"):
      Given("a CPU in random state with LD(N)Z r1,r2 as next instruction")
      And("r2 is PC")
      doTestNOP(TestUtils.createRandomStateCpu.clearZ(), INSTR_LDZ_RR(0, 0xA))
      doTestNOP(TestUtils.createRandomStateCpu.setZ(), INSTR_LDNZ_RR(0, 0xB))

    Scenario("copy from register to PC when Z flag matches instruction condition (jump conditional)"):
      Given("a CPU in random state with LD(N)Z r1,r2 as next instruction")
      And("r2 is PC and r1 is not PC")
      doTestLDx_R_PC(TestUtils.createRandomStateCpu.setZ(), INSTR_LDZ_RR(_, 0), r => r != 0 && r != 2)
      doTestLDx_R_PC(TestUtils.createRandomStateCpu.clearZ(), INSTR_LDNZ_RR(_, 0), r => r != 0 && r != 2)

    Scenario("copy from register to PC when Z flag does not match instruction condition"):
      Given("a CPU in random state with LD(N)Z r1,r2 as next instruction")
      And("r2 is PC and r1 is not PC")
      doTestNOP(TestUtils.createRandomStateCpu.clearZ(), INSTR_LDZ_RR(0xC, 0))
      doTestNOP(TestUtils.createRandomStateCpu.setZ(), INSTR_LDNZ_RR(0xD, 0))

    Scenario("read memory to register when Z flag matches instruction condition"):
      Given("a CPU in random state with LD(N)Z (r1),r2 as next instruction")
      And("r2 is not PC")
      doTestLDx_MR(TestUtils.createRandomStateCpuWithMemory().setZ(), INSTR_LDZ_MR.apply, r => r != 0 && r != 2)
      doTestLDx_MR(TestUtils.createRandomStateCpuWithMemory().clearZ(), INSTR_LDNZ_MR.apply, r => r != 0 && r != 2)

    Scenario("read memory to register when Z flag does not match instruction condition"):
      Given("a CPU in random state with LD(N)Z (r1),r2 as next instruction")
      And("r2 is not PC")
      doTestNOP(TestUtils.createRandomStateCpuWithMemory().clearZ(), INSTR_LDZ_MR(0x5,0x6))
      doTestNOP(TestUtils.createRandomStateCpuWithMemory().setZ(), INSTR_LDNZ_MR(0x7,0x8))

    Scenario("read memory to PC when Z flag matches instruction condition (jump conditional)"):
      Given("a CPU in random state with LD(N)Z (r1),r2 as next instruction")
      And("r2 is PC")
      doTestLDx_M_PC(TestUtils.createRandomStateCpuWithMemory().setZ(), INSTR_LDZ_MR(_, 0))
      doTestLDx_M_PC(TestUtils.createRandomStateCpuWithMemory().clearZ(), INSTR_LDNZ_MR(_, 0))

    Scenario("read memory to PC when Z flag does not match instruction condition"):
      Given("a CPU in random state with LD(N)Z (r1),r2 as next instruction")
      And("r2 is PC")
      doTestNOP(TestUtils.createRandomStateCpuWithMemory().clearZ(), INSTR_LDZ_MR(0x1234, 0))
      doTestNOP(TestUtils.createRandomStateCpuWithMemory().setZ(), INSTR_LDNZ_MR(0x1234, 0))

    Scenario("write memory when Z flag matches instruction condition"):
      Given("a CPU in random state with LD(N)Z r1,(r2) as next instruction")
      doTestLDx_RM(TestUtils.createRandomStateCpuWithMemory().setZ(), INSTR_LDZ_RM.apply)
      doTestLDx_RM(TestUtils.createRandomStateCpuWithMemory().clearZ(), INSTR_LDNZ_RM.apply)

    Scenario("write memory when Z flag does not match instruction condition"):
      Given("a CPU in random state with LD(N)Z r1,(r2) as next instruction")
      doTestNOP(TestUtils.createRandomStateCpuWithMemory().clearZ(), INSTR_LDZ_RM(0xA,0xB))
      doTestNOP(TestUtils.createRandomStateCpuWithMemory().setZ(), INSTR_LDNZ_RM(0xA,0xB))

  Feature("ALU"):
    Scenario("handle ADD (r1+=r2"):
      Given("a CPU in random state with ADD r1,r2 as next instruction and r1>2 (excl. pc, sp, fl)")
      doTestALU(TestUtils.createRandomStateCpu, INSTR_ADD.apply, AluOp.Add, _ > 2)

    Scenario("handle SUB (r1-=r2"):
      Given("a CPU in random state with SUB r1,r2 as next instruction and r1>2 (excl. pc, sp, fl)")
      doTestALU(TestUtils.createRandomStateCpu, INSTR_SUB.apply, AluOp.Sub, _ > 2)

  Scenario("handle AND (r1&=r2"):
    Given("a CPU in random state with AND r1,r2 as next instruction and r1>2 (excl. pc, sp, fl)")
    doTestALU(TestUtils.createRandomStateCpu, INSTR_AND.apply, AluOp.And, _ > 2)

  Scenario("handle OR (r1|=r2"):
    Given("a CPU in random state with OR r1,r2 as next instruction and r1>2 (excl. pc, sp, fl)")
    doTestALU(TestUtils.createRandomStateCpu, INSTR_OR.apply, AluOp.Or, _ > 2)

  Scenario("handle XOR (r1^=r2"):
    Given("a CPU in random state with XOR r1,r2 as next instruction and r1>2 (excl. pc, sp, fl)")
    doTestALU(TestUtils.createRandomStateCpu, INSTR_XOR.apply, AluOp.Xor, _ > 2)

  private def doTestNOP(cpuStart: Cpu, instr: Instruction): Unit =
    forAll(TestUtils.addressGen):
      pc =>
        val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, instr.value)
        When("executed")
        val cpuAfter = cpuBefore.handleNext
        Then("PC is increased by 1")
        assert(cpuAfter.pc == (pc + 1).toShort)
        And("no other change is made")
        assert((1 to 0xF).forall(r => cpuAfter.register(r) == cpuBefore.register(r)))
        assert((0 to 0xFFFF).forall(m => cpuAfter.memory(m) == cpuBefore.memory(m)))

  private def doTestLDx_AL(cpuStart:Cpu, instr:Short => Instruction):Unit =
    forAll(TestUtils.addressGen, TestUtils.byteValueGen):
      (pc, imm) =>
        val aBefore = cpuStart.a
        val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, instr(imm).value)
        When("executed")
        val cpuAfter = cpuBefore.handleNext
        Then("A (low) is loaded with operand")
        assert(cpuAfter.a == ((aBefore & 0xFF00) | imm).toShort)
        And("PC is increased by 1")
        assert(cpuAfter.pc == (pc + 1).toShort)

  private def doTestLDx_AH(cpuStart: Cpu, instr: Short => Instruction): Unit =
    forAll(TestUtils.addressGen, TestUtils.byteValueGen):
      (pc, imm) =>
        val aBefore = cpuStart.a
        val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, instr(imm).value)
        When("executed")
        val cpuAfter = cpuBefore.handleNext
        Then("A (high) is loaded with operand")
        assert(cpuAfter.a == ((aBefore & 0x00FF) | (imm << 8)).toShort)
        And("PC is increased by 1")
        assert(cpuAfter.pc == (pc + 1).toShort)

  private def doTestLDx_RR(cpuStart: Cpu, instr: (Short, Short) => Instruction, registerPredicate: (Short,Short) => Boolean): Unit =
    forAll(TestUtils.addressGen, TestUtils.registerIndexGen, TestUtils.registerIndexGen):
      (pc, r1, r2) =>
        whenever(registerPredicate(r1,r2)):
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, instr(r1, r2).value)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("register 2 is loaded with value from register 1")
          //println(f"R1($r1): ${cpuAfter.register(r1)} R2($r2) after: ${cpuAfter.register(r2)} R2 before: ${cpuBefore.register(r2)}")
          assert(cpuAfter.register(r2) == cpuAfter.register(r1))
          And("PC is increased by 1")
          assert(cpuAfter.pc == (pc + 1).toShort)

  private def doTestLDx_PC_R(cpuStart: Cpu, instr: Short => Instruction, registerPredicate: Short => Boolean): Unit =
    forAll(TestUtils.addressGen, TestUtils.registerIndexGen):
      (pc, r2) =>
        whenever(registerPredicate(r2)):
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, instr(r2).value)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("register 2 is loaded with value from PC (before increase)")
          //println(f"R1(0): ${cpuAfter.pc} R2($r2) after: ${cpuAfter.register(r2)} R2 before: ${cpuBefore.register(r2)}")
          assert(cpuAfter.register(r2) == pc.toShort)
          And("PC is increased by 1")
          assert(cpuAfter.pc == (pc + 1).toShort)

  private def doTestLDx_R_PC(cpuStart: Cpu, instr: Short => Instruction, registerPredicate: Short => Boolean): Unit =
    forAll(TestUtils.addressGen, TestUtils.registerIndexGen):
      (pc, r1) =>
        whenever(registerPredicate(r1)):
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, instr(r1).value)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("PC is loaded with value from register 1")
          //println(f"pc: $pc R1($r1): ${cpuAfter.register(r1)} R2(0) after: ${cpuAfter.pc} R2 before: ${cpuBefore.pc}")
          assert(cpuAfter.pc == cpuAfter.register(r1))

  private def doTestLDx_MR(cpuStart: Cpu, instr: (Short, Short) => Instruction, register2Predicate: Short => Boolean): Unit =
    forAll(TestUtils.addressGen, TestUtils.registerIndexGen, TestUtils.registerIndexGen):
      (pc, r1, r2) =>
        //NOTE: test only non-zero memory as there may be too many zeros (memory may not be fully initialized for performance reasons)
        whenever(register2Predicate(r2) && cpuStart.memory(cpuStart.register(r1)) != 0):
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, instr(r1, r2).value)
          When("executed")
          val sourceMemory = cpuBefore.memory(cpuBefore.register(r1)) // this is to copy value in case both registers are equal
          val cpuAfter = cpuBefore.handleNext
          Then("register 2 is loaded with memory contents at address from register 1")
          //println(f"(R1)($r1): ${cpuAfter.register(r1)} ${cpuAfter.memory(cpuAfter.register(r1))} R2($r2) after: ${cpuAfter.register(r2)} R2 before: ${cpuBefore.register(r2)}")
          assert(cpuAfter.register(r2) == sourceMemory)
          And("PC is increased by 1")
          assert(cpuAfter.pc == (pc + 1).toShort)

  private def doTestLDx_M_PC(cpuStart: Cpu, instr: Short => Instruction): Unit =
    forAll(TestUtils.addressGen, TestUtils.registerIndexGen):
      (pc, r1) =>
        whenever(cpuStart.memory(cpuStart.register(r1)) != 0):
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, instr(r1).value)
          When("executed")
          val sourceMemory = cpuBefore.memory(cpuBefore.register(r1)) // this is to copy value in case both registers are equal
          val cpuAfter = cpuBefore.handleNext
          Then("PC is loaded with memory contents at address from register 1")
          //println(f"(R1)($r1): ${cpuAfter.register(r1)} ${cpuAfter.memory(cpuAfter.register(r1))} R2(0) after: ${cpuAfter.pc} R2 before: ${cpuBefore.pc}")
          assert(cpuAfter.pc == sourceMemory)

  private def doTestLDx_RM(cpuStart: Cpu, instr: (Short, Short) => Instruction): Unit =
    forAll(TestUtils.addressGen, TestUtils.registerIndexGen, TestUtils.registerIndexGen):
      (pc, r1, r2) =>
        whenever(cpuStart.memory(cpuStart.register(r1)) != 0):
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, instr(r1, r2).value)
          val r1Before = cpuBefore.register(r1) // this is to copy value in case one of registers is PC (it is increased)
          val r2Before = cpuBefore.register(r2)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("memory at address from register 2 is loaded with register 1")
          //println(f"(R1)($r1): ${cpuAfter.register(r1)} R2($r2) after: ${cpuAfter.register(r2)} ${cpuAfter.memory(cpuAfter.register(r2))}  R2 before: ${cpuBefore.memory(cpuBefore.register(r2))}")
          assert(cpuAfter.memory(r2Before) == r1Before)
          And("PC is increased by 1")
          assert(cpuAfter.pc == (pc + 1).toShort)

  private def doTestALU(cpuStart: Cpu, instr: (Short, Short) => Instruction, oper: AluOp, registerPredicate: Short => Boolean): Unit =
    forAll(TestUtils.addressGen, TestUtils.registerIndexGen, TestUtils.registerIndexGen):
      (pc, r1, r2) =>
        whenever(registerPredicate(r1)):
          val cpuBefore = cpuStart.setPc(pc.toShort).writeMemory(pc, instr(r1, r2).value)
          val r1Before = cpuBefore.register(r1)
          val r2Before = cpuBefore.register(r2)
          When("executed")
          val cpuAfter = cpuBefore.handleNext
          Then("r1 is changed with according to ALU operation")
          //println(f"R1($r1) after: ${cpuAfter.register(r1)}  R2 before: ${cpuBefore.register(r1)} R2($r2) after: ${cpuAfter.register(r2)} R2 before: ${cpuBefore.register(r2)}")
          val res= Alu(r1Before, r2Before, cpuBefore.fl, oper)
          assert(cpuAfter.register(r1) == res._1)
          assert(cpuAfter.fl == res._2)
          And("PC is increased by 1")
          assert(cpuAfter.pc == (pc + 1).toShort)

