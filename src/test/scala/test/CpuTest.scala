package org.kr.cpu
package test

import org.scalactic.anyvals.PosZInt
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CpuTest extends AnyFeatureSpec with GivenWhenThen with ScalaCheckPropertyChecks:
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50, maxDiscardedFactor = 30.0, minSize = PosZInt(100))
  
  Feature("CPU reset sequence"):
    Scenario("reset CPU"):
      Given("A cpu instance in random state")
      val cpuInit = TestUtils.createRandomStateCpu
      When("reset is invoked")
      val cpuReset = cpuInit.reset
      Then("Cpu is in a initial state")
      forAll(TestUtils.registerIndexGen):
        index => assert(cpuReset.register(index) == 0)

  Feature("CPU register basic operations"):
    Scenario("set register value"):
      Given("a cpu instance in random state")
      val cpuInit = TestUtils.createRandomStateCpu
      When("a register is set to a given value")
      Then("the same value can be read from register")
      forAll(TestUtils.registerIndexGen,TestUtils.anyValueGen):
        (index, value) =>
          val cpuSet = cpuInit.setReg(index,value)
          assert(cpuSet.register(index)==value)

    Scenario("increase PC"):
      Given("a cpu instance in random state")
      forAll(TestUtils.addressGen):
        pc => whenever(pc < 0xFFFF):
          val cpuInit = TestUtils.createRandomStateCpu.setPc(pc.toShort)
          When("PC is increased")
          val cpuIncPC = cpuInit.incPC
          Then("new PC value is equal to old value increased by 1")
          assert(cpuIncPC.pc == cpuInit.pc + 1)

    Scenario("overflow PC"):
      Given("a cpu instance in random state and PC set to maximum value")
      val cpuInit = TestUtils.createRandomStateCpu.setPc(0xFFFF.toShort)
      assert(cpuInit.pc == 0xFFFF.toShort)
      When("PC is increased")
      val cpuIncPC = cpuInit.incPC
      Then("new PC value is 0")
      assert(cpuIncPC.pc == 0x0000)

    Scenario("increase SP"):
      Given("a cpu instance in random state")
      forAll(TestUtils.addressGen):
        sp => whenever(sp < 0xFFFF)
          val cpuInit = TestUtils.createRandomStateCpu.setSp(sp.toShort)
          When("SP is increased")
          val cpuIncSP = cpuInit.incSP
          Then("new SP value is equal to old value increased by 1")
          assert(cpuIncSP.sp == cpuInit.sp + 1)

    Scenario("overflow SP when increasing"):
      Given("a cpu instance in random state and SP set to maximum value")
      val cpuInit = TestUtils.createRandomStateCpu.setSp(0xFFFF.toShort)
      When("SP is increased")
      val cpuIncSP = cpuInit.incSP
      Then("new SP value is 0")
      assert(cpuIncSP.sp == 0x0000)

    Scenario("decrease SP"):
      Given("a cpu instance in random state")
      forAll(TestUtils.addressGen):
        sp =>
          val cpuInit = TestUtils.createRandomStateCpu.setSp(sp.toShort)
          When("SP is decreased")
          val cpuIncSP = cpuInit.decSP
          Then("new SP value is equal to old value decreased by 1")
          assert(cpuIncSP.sp == cpuInit.sp - 1)

    Scenario("overflow SP when decreasing"):
      Given("a cpu instance in random state and SP set to 0")
      val cpuInit = TestUtils.createRandomStateCpu.setSp(0.toShort)
      When("SP is decreased")
      val cpuDecSP = cpuInit.decSP
      Then("new SP value is 0xFFFF (-1)")
      assert(cpuDecSP.sp == -1)

  Feature("memory operations"):
    Scenario("read from / write to memory"):
      Given("a cpu instance in random state")
      And("memory instance initialized with some values at some addresses")
      val cpuInit =  TestUtils.createRandomStateCpu
      val memoryPairs = TestUtils.generateMemoryContents()
      val cpuMem = memoryPairs.foldLeft(cpuInit)({case (cpu,pair) => cpu.writeMemory(pair._1,pair._2) })
      When("the same address is read from memory")
      Then("value is the same as previously initialized")
      memoryPairs.foreach(pair => assert(cpuMem.memory(pair._1) == pair._2))

  Feature("ALU operations"):
    Scenario("add w/o carry"):
      Given("two different (small) positive numbers")
      When("added")
      Then("result is sum and flags are off")
      forAll(TestUtils.smallPositiveValueGen,TestUtils.smallPositiveValueGen):
        (a, b) => assert(Alu(a,b,0xFFFF.toShort,ADD) == (a+b,0xFFF8.toShort))

    Scenario("add the same numbers with opposite signs"):
      Given("a (small) positive number")
      When("the same numbers are added with opposite signs")
      Then("result is 0 and zero flag is set")
      forAll(TestUtils.smallPositiveValueGen):
        a => assert(Alu(a, (-a).toShort, 0xFF00.toShort, ADD) == (0, 0xFF01.toShort))

    Scenario("add with carry"):
      Given("two different (large) positive numbers")
      When("added")
      Then("result is sum trimmed to 16b and carry flag is set")
      //NOTE: forAll may sometimes not respect Gen min/max values due to defect in ScalaCheck
      forAll(TestUtils.largePositiveValueGen, TestUtils.largePositiveValueGen):
        (a, b) => assert(Alu(a,b,0xFF00.toShort,ADD) == ((a+b).toShort,0xFF04.toShort))

    Scenario("add negative numbers w/o borrow"):
      Given("two different (small) negative numbers")
      When("added")
      Then("result is (negative) sum trimmed to 16b and flags are off")
      forAll(TestUtils.smallPositiveValueGen, TestUtils.smallPositiveValueGen):
        (a, b) => assert(Alu((-a).toShort, (-b).toShort, 0xFF00.toShort, ADD) == ((-(a + b)).toShort, 0xFF00.toShort))

    Scenario("add negative numbers with borrow"):
      Given("two different (large) negative numbers")
      When("added")
      Then("result is sum trimmed to (positive) 16b and borrow flag is set")
      forAll(TestUtils.largePositiveValueGen, TestUtils.largePositiveValueGen):
        (a, b) => assert(Alu((-a).toShort, (-b).toShort, 0xFF00.toShort, ADD) == ((-(a + b)).toShort, 0xFF02.toShort))

    Scenario("add large numbers with opposite signs"):
      Given("two different (large) numbers with opposite signs")
      When("added")
      Then("result is sum trimmed to 16b and flags are off")
      forAll(TestUtils.largePositiveValueGen, TestUtils.largePositiveValueGen):
        (a, b) => assert(Alu(a, (-b).toShort, 0xFF00.toShort, ADD) == ((a - b).toShort, 0xFF00.toShort))

    Scenario("express Sub as Add with second operand negated"):
      Given("two different numbers")
      When("subtracted")
      Then("result is the same as sum with second operand negated")
      forAll(TestUtils.anyValueGen, TestUtils.anyValueGen):
        (a, b) => assert(Alu(a, b, 0xFF00.toShort, SUB) == Alu(a, (-b).toShort, 0xFF00.toShort, ADD))

    Scenario("express Inc as Add with second operand 1"):
      Given("a number")
      When("increased")
      Then("result is the same as sum with 1")
      forAll(TestUtils.anyValueGen):
        a => assert(Alu(a, 1, 0xFF00.toShort, ADD) == Alu(a, 0, 0xFF00.toShort, INC))

    Scenario("express Dec as Add with second operand -1"):
      Given("a number")
      When("increased")
      Then("result is the same as sum with -1")
      forAll(TestUtils.anyValueGen):
        a => assert(Alu(a, -1, 0xFF00.toShort, ADD) == Alu(a, 0, 0xFF00.toShort, DEC))

    Scenario("bitwise And"):
      Given("two different numbers")
      When("and'ed")
      Then("result is bitwise And")
      forAll(TestUtils.anyValueGen,TestUtils.anyValueGen):
        (a, b) =>
          assert(Alu(a, b, 0xFFFF.toShort, AND) == ((a & b).toShort, (0xFFFE | (if ((a & b)==0) 1 else 0)).toShort))

    Scenario("bitwise And with 0"):
      Given("any number")
      When("and'ed with 0")
      Then("result is 0 and zero flag is set")
      forAll(TestUtils.anyValueGen):
        a => assert(Alu(a, 0, 0xFFFF.toShort, AND) == (0, 0xFFFF.toShort))

    Scenario("bitwise Or"):
      Given("two different numbers")
      When("or'ed")
      Then("result is bitwise Or")
      forAll(TestUtils.anyValueGen, TestUtils.anyValueGen):
        (a, b) => whenever(a!=0 || b!=0):
          assert(Alu(a, b, 0xFFFF.toShort, OR) == ((a | b).toShort, 0xFFFE.toShort))

    Scenario("bitwise Or of two 0s"):
      Given("zero as both operands")
      When("or'ed")
      Then("result is 0 and zero flag is set")
      assert(Alu(0, 0, 0xFFFF.toShort, OR) == (0, 0xFFFF.toShort))

    Scenario("bitwise Xor"):
      Given("two different numbers")
      When("xor'ed")
      Then("result is bitwise Xor")
      forAll(TestUtils.anyValueGen, TestUtils.anyValueGen):
        (a, b) => whenever(a != b):
          assert(Alu(a, b, 0xFFFF.toShort, XOR) == ((a ^ b).toShort, 0xFFFE.toShort))

    Scenario("bitwise Xor or same numbers"):
      Given("any same numbers")
      When("xor'ed")
      Then("result is 0 and zero flag is set")
      forAll(TestUtils.anyValueGen):
        a => assert(Alu(a, a, 0xFFFF.toShort, XOR) == (0, 0xFFFF.toShort))

    Scenario("compare different numbers"):
      Given("two different numbers")
      When("compared")
      Then("zero flag is not set")
      forAll(TestUtils.anyValueGen, TestUtils.anyValueGen):
        (a, b) => whenever(a != b): 
          assert(Alu(a, b, 0xFFFF.toShort, CMP)._2 == 0xFFFE.toShort)

    Scenario("compare same numbers"):
      Given("two same numbers")
      When("compared")
      Then("zero flag is set")
      forAll(TestUtils.anyValueGen):
        a => assert(Alu(a, a, 0xFFFF.toShort, CMP)._2 == 0xFFFF.toShort)

  Feature("decode opcode"):
    Scenario("decode all opcodes"):
      Given("a list of all opcodes (0..F) including illegal")
      val opcodeNum=(0 to 0xF).toVector
      When("decoded")
      Then("resulting opcode has the same number")
      opcodeNum.foreach(num => assert(Opcode.codes(num).code == num))

  Feature("decode address mode"):
    Scenario("decode all address modes"):
      Given("a list of all address modes (0..F) including illegal")
      val modeNum = (0 to 0xF).toVector
      When("decoded")
      Then("resulting address mode has the same number")
      modeNum.foreach(num => assert(AddressMode.codes(num).code == num))

  Feature("In/Out operations"):
    Scenario("output a value to each port"):
      Given("A cpu instance in random state")
      val cpuInit = TestUtils.createRandomStateCpu
      When("a value is outputted")
      Then("output file contains proper register content")
      forAll(TestUtils.portGen,TestUtils.registerIndexGen):
        (port,reg) =>
          val value = cpuInit.register(reg)
          val cpuAfter = cpuInit.output(port, value)
          assert(cpuAfter.outputFile.lastPort == port)
          assert(cpuAfter.outputFile.lastValue == value)
          assert(cpuAfter.outputFile(port,0) == value)

    Scenario("output multiple values"):
      Given("A cpu instance in random state")
      val cpuInit = TestUtils.createRandomStateCpu
      When("value are outputted")
      // output A, B ...
      val cpuOutput = (0 to 9).foldLeft(cpuInit)((cpu,value) => cpu.output(0xB, ('A'.toShort+value).toShort))
      Then("output file contains the same values")
      assert(cpuOutput.outputFile.files(0xB).size==10)
      val outText = (0 to 9).foldLeft("")((t,v)=> t + cpuOutput.outputFile.files(0xB)(v).toChar)
      assert(outText == "ABCDEFGHIJ")

    Scenario("input a value from each port"):
      Given("A cpu instance in reset state (all registers set to 0)")
      val cpuInit = TestUtils.createResetCpu
      When("a value is inputted")
      Then("target register file contains proper value")
      forAll(TestUtils.portGen,TestUtils.registerIndexGen, TestUtils.anyValueGen):
        (port,reg, value) => whenever(reg>0)
          val inputFileSet = cpuInit.inputFile.attachPort(port, InputPortVector.single(value))
          val cpuWithInput = cpuInit.copy(inputFile = inputFileSet)
          val cpuAfter = cpuWithInput.input(port, reg)
          assert(cpuAfter.register(reg) == value)


