package org.kr.cpu
package test

import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ProgramTest extends AnyFeatureSpec with GivenWhenThen {
  Feature("Sequential register operations"):
    pending

  Feature("Sequential memory operations"):
    pending

  Feature("Call and return (work in progress"):
    Scenario("Call a routine and return to correct address"):
      Given("a program in memory with call and return")
      val cpuInit = TestUtils.createResetCpu
      assert(cpuInit.register(4) == 0)
      assert(cpuInit.register(5) == 0)
      assert(cpuInit.register(6) == 0)
      val cpuProg = cpuInit
        .writeMemory(0x0000,INSTR_LD_AL(0x01).value)
        .writeMemory(0x0001,INSTR_LD_AH(0x02).value)
        .writeMemory(0x0002,INSTR_LD_RR(3,4).value)
        // CALL
        .writeMemory(0x0003, INSTR_DEC_SP().value) // DEC SP
        .writeMemory(0x0004, INSTR_LD_AL(0x0A).value) // return address to A (set at compile time)
        .writeMemory(0x0005, INSTR_LD_AH(0x00).value)
        .writeMemory(0x0006, INSTR_LD_RM(3,1).value) // LD A => (SP) - push return address to stack
        .writeMemory(0x0007, INSTR_LD_AL(0x00).value) // call address
        .writeMemory(0x0008, INSTR_LD_AH(0x01).value)
        .writeMemory(0x0009, INSTR_LD_RR(3,0).value) // LD A => PC - jump to subroutine
        // after return
        .writeMemory(0x000A,INSTR_LD_AL(0x03).value)
        .writeMemory(0x000B,INSTR_LD_AH(0x04).value)
        .writeMemory(0x000C,INSTR_LD_RR(3,6).value) // END
        // subroutine
        .writeMemory(0x0100, INSTR_LD_AL(0x34).value)
        .writeMemory(0x0101, INSTR_LD_AH(0x12).value)
        .writeMemory(0x0102, INSTR_LD_RR(3,5).value)
        // RETURN
        .writeMemory(0x0103, INSTR_LD_MR(1,3).value) // LD (SP) => A - pop return address from stack
        .writeMemory(0x0104, INSTR_INC_SP().value)
        .writeMemory(0x0105, INSTR_LD_RR(3,0).value) // LD A => PC - jump to return address


      When("executed")
      val cpuExec = cpuProg handleNext 19
      Then("call and return is reflected in register state")
      //assert(cpuExec.register(3) == 0x0201)
      assert(cpuExec.register(4) == 0x0201) // set at the beginning
      assert(cpuExec.register(5) == 0x1234) // set in subroutine
      assert(cpuExec.register(6) == 0x0403) // set after return
      assert(cpuExec.sp == 0) // SP was decremented and incremented
      assert(cpuExec.memory(0xFFFF) == 0x000A) // traces of return address
      assert(cpuExec.pc == 0x000D) // next address after end of the program


  Feature("A loop with register as counter"):
    pending
}
