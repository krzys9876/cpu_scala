package org.kr.cpu
package test

import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ProgramTest extends AnyFeatureSpec with GivenWhenThen:
  Feature("Sequential register operations"):
    pending

  Feature("Sequential memory operations"):
    pending

  Feature("Call and return (work in progress"):
    Scenario("Call a subroutine and return to correct address"):
      Given("a program in memory with call and return")
      val cpuInit = TestUtils.createResetCpu
      assert(cpuInit.register(4) == 0)
      assert(cpuInit.register(5) == 0)
      assert(cpuInit.register(6) == 0)
      val cpuProg = cpuInit
        .writeMemoryMulti(0x0000,MACRO.LD_R(0x0201,4))
        // CALL
        .writeMemoryMulti(0x0003, MACRO.CALL(0x0003,0x0100))
        // after return
        .writeMemoryMulti(0x000A,MACRO.LD_R(0x0403,6)) // END
        // subroutine
        .writeMemoryMulti(0x0100, MACRO.LD_R(0x1234,5))
        // RETURN
        .writeMemoryMulti(0x0103, MACRO.RET)
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

    Scenario("Call a nested subroutine and return to correct address"):
      Given("a program in memory with nested call and return")
      val cpuInit = TestUtils.createResetCpu
      assert(cpuInit.register(4) == 0)
      assert(cpuInit.register(5) == 0)
      assert(cpuInit.register(6) == 0)
      assert(cpuInit.register(7) == 0)
      val cpuProg = cpuInit
        .writeMemoryMulti(0x0000, MACRO.LD_R(0x4444, 4))
        // CALL
        .writeMemoryMulti(0x0003, MACRO.CALL(0x0003, 0x1000))
        // after return
        .writeMemoryMulti(0x000A, MACRO.LD_R(0x7777, 7))
        // END
        // subroutine
        .writeMemoryMulti(0x1000, MACRO.LD_R(0x5555, 5))
        // nested CALL
        .writeMemoryMulti(0x1003, MACRO.CALL(0x1003, 0x2000))
        // RETURN
        .writeMemoryMulti(0x100A, MACRO.RET)
        // nested subroutine
        .writeMemoryMulti(0x2000, MACRO.LD_R(0x6666, 6))
        // RETURN
        .writeMemoryMulti(0x2003, MACRO.RET)
      When("executed")
      val cpuExec = cpuProg handleNext 32
      Then("call and return is reflected in register state")
      assert(cpuExec.register(4) == 0x4444) // set at the beginning
      assert(cpuExec.register(5) == 0x5555) // set in subroutine
      assert(cpuExec.register(6) == 0x6666) // set in nested subroutine
      assert(cpuExec.register(7) == 0x7777) // set after return
      assert(cpuExec.sp == 0) // SP was decremented and incremented
      assert(cpuExec.memory(0xFFFF) == 0x000A) // traces of return address from subroutine
      assert(cpuExec.memory(0xFFFE) == 0x100A) // traces of return address from nested subroutine
      assert(cpuExec.pc == 0x000D) // next address after end of the program

  Feature("A loop with register as counter"):
    Scenario("Increment a register in a loop"):
      Given("a program in memory with a loop")
      val cpuInit = TestUtils.createResetCpu
      assert(cpuInit.register(0xA) == 0)
      assert(cpuInit.register(0xB) == 0)
      val cpuProg = cpuInit
        .writeMemoryMulti(0x0000, MACRO.LD_R(0x0004, 0xA)) // counter
        .writeMemoryMulti(0x0003, MACRO.LD_R(0x0000, 0xB)) // incremented variable
        // loop
        .writeMemory(0x0004, INSTR_INC(0xB).value) // increment variable
        .writeMemoryMulti(0x0005, MACRO.LD_A(0x0004)) // jump address
        .writeMemory(0x0007, INSTR_DEC(0xA).value) // decrement counter
        .writeMemory(0x0008, INSTR_JMPNZ_A().value) // jump if not zero
        // end loop
        .writeMemoryMulti(0x0009, MACRO.LD_R(0x0C0C, 0xC))
        // END
      When("executed")
      val cpuExec = cpuProg handleNext 4+5*4+3
      Then("call and return is reflected in register state")
      assert(cpuExec.register(0xA) == 0) // counter = 0
      assert(cpuExec.register(0xB) == 4) // variable = 4
      assert(cpuExec.pc == 0x000C) // next address after end of the program
