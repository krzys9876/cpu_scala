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
        .writeMemoryMulti(0x0000,Vector(
          INSTR_LD_AL(0x01).value,INSTR_LD_AH(0x02).value,
          INSTR_LD_RR(3,4).value))
        // CALL
        .writeMemoryMulti(0x0003, Vector(
          INSTR_DEC_SP().value, // DEC SP
          INSTR_LD_AL(0x0A).value,INSTR_LD_AH(0x00).value, // return address to A (determined at compile time)
          INSTR_LD_RM(3,1).value, // LD A => (SP) - push return address to stack
          INSTR_LD_AL(0x00).value, INSTR_LD_AH(0x01).value, // call address
          INSTR_JMP_A().value)) // LD A => PC - jump to subroutine
        // after return
        .writeMemoryMulti(0x000A,Vector(
          INSTR_LD_AL(0x03).value,INSTR_LD_AH(0x04).value,
          INSTR_LD_RR(3,6).value)) // END
        // subroutine
        .writeMemoryMulti(0x0100, Vector(
          INSTR_LD_AL(0x34).value,INSTR_LD_AH(0x12).value,
          INSTR_LD_RR(3,5).value))
        // RETURN
        .writeMemoryMulti(0x0103, Vector(INSTR_LD_MR(1,3).value, // LD (SP) => A - pop return address from stack
          INSTR_INC_SP().value,
          INSTR_JMP_A().value)) // LD A => PC - jump to return address
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
