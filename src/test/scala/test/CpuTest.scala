package org.kr.cpu
package test

import org.scalacheck.Gen
import org.scalactic.anyvals.PosZInt
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class CpuTest extends AnyFeatureSpec with GivenWhenThen with ScalaCheckPropertyChecks:
  val testCpuHandler:CpuHandler = CpuHandlerImmutable

  val registerIndexGen:Gen[Short] = Gen.choose(0.toShort,15.toShort)
  val addressGen:Gen[Int] = Gen.choose(0,0xFFFF)
  val valueGen:Gen[Short] = Gen.choose((-0x8000).toShort,0x7FFF.toShort)

  // NOTE: forAll after first failed test may ignore conditions in generator
  // Always verify the first failed test
  val smallPositiveValueGen:Gen[Short] = Gen.choose(1.toShort,0x3FFF.toShort)
  val largePositiveValueGen:Gen[Short] = Gen.choose(0x4000.toShort,0x7FFF.toShort)
  val anyValueGen:Gen[Short] = Gen.choose(Short.MinValue,Short.MaxValue)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50, maxDiscardedFactor = 30.0, minSize = PosZInt(100))


  Feature("CPU reset sequence"):
    Scenario("reset CPU"):
      Given("A cpu instance in random state")
      val cpuInit = createRandomStateCpu
      When("reset is invoked")
      val cpuReset = cpuInit.reset
      Then("Cpu is in a initial state")
      forAll(registerIndexGen):
        index => assert(cpuReset.register(index) == 0)

  Feature("CPU register basic operations"):
    Scenario("set register value"):
      Given("a cpu instance in random state")
      val cpuInit = createRandomStateCpu
      When("a register is set to a given value")
      Then("the same value can be read from register")
      forAll(registerIndexGen,valueGen):
        (index, value) =>
          val cpuSet = cpuInit.setReg(index,value)
          assert(cpuSet.register(index)==value)

    Scenario("increase PC"):
      Given("a cpu instance in random state")
      val cpuInit = createRandomStateCpu.setPc(0x1234.toShort)
      val pcInit = cpuInit.pc
      When("PC is increased")
      val cpuIncPC = cpuInit.incPC
      val pcInc = cpuIncPC.pc
      Then("new PC value is equal to old value increased by 1")
      assert(pcInc==pcInit+1)

    Scenario("overflow PC"):
      Given("a cpu instance in random state and PC set to maximum value")
      val cpuInit = createRandomStateCpu.setPc(0xFFFF.toShort)
      assert(cpuInit.pc == -1) //NOTE: this is a signed type
      When("PC is increased")
      val cpuIncPC = cpuInit.incPC
      Then("new PC value is 0")
      assert(cpuIncPC.pc == 0x0000)

    Scenario("increase SP"):
      Given("a cpu instance in random state")
      val cpuInit = createRandomStateCpu.setSp(0x5432.toShort)
      val spInit = cpuInit.sp
      When("SP is increased")
      val cpuIncSP = cpuInit.incSP
      val spInc = cpuIncSP.sp
      Then("new SP value is equal to old value increased by 1")
      assert(spInc == spInit + 1)

    Scenario("overflow SP when increasing"):
      Given("a cpu instance in random state and SP set to maximum value")
      val cpuInit = createRandomStateCpu.setSp(0xFFFF.toShort)
      When("SP is increased")
      val cpuIncSP = cpuInit.incSP
      Then("new SP value is 0")
      assert(cpuIncSP.sp == 0x0000)

    Scenario("decrease SP"):
      Given("a cpu instance in random state")
      val cpuInit = createRandomStateCpu.setSp(0x8765.toShort)
      val spInit = cpuInit.sp
      When("SP is decreased")
      val cpuDecSP = cpuInit.decSP
      val spInc = cpuDecSP.sp
      Then("new SP value is equal to old value decreased by 1")
      assert(spInc == spInit - 1)

    Scenario("overflow SP when decreasing"):
      Given("a cpu instance in random state and SP set to 0")
      val cpuInit = createRandomStateCpu.setSp(0.toShort)
      When("SP is decreased")
      val cpuDecSP = cpuInit.decSP
      Then("new SP value is 0xFFFF (-1)")
      assert(cpuDecSP.sp == -1)

  Feature("memory operations"):
    Scenario("read from / write to memory"):
      Given("a cpu instance in random state")
      And("memory instance initialized with some values at some addresses")
      val cpuInit = createRandomStateCpu
      val memoryPairs = generateMemoryContents
      val cpuMem = memoryPairs.foldLeft(cpuInit)({case (cpu,pair) => cpu.writeMemory(pair._1,pair._2) })
      When("the same address is read from memory")
      Then("value is the same as previously initialized")
      memoryPairs.foreach(pair => assert(cpuMem.memory(pair._1) == pair._2))

  Feature("ALU operations"):
    Scenario("add w/o carry"):
      Given("two different (small) positive numbers")
      When("added")
      Then("result is sum and flags are off")
      forAll(smallPositiveValueGen,smallPositiveValueGen):
        (a, b) => assert(Alu(a,b,0xFFFF.toShort,AluOp.Add) == (a+b,0xFFF8.toShort))

    Scenario("add the same numbers with opposite signs"):
      Given("a (small) positive number")
      When("the same numbers are added with opposite signs")
      Then("result is 0 and zero flag is set")
      forAll(smallPositiveValueGen):
        a => assert(Alu(a, (-a).toShort, 0xFF00.toShort, AluOp.Add) == (0, 0xFF01.toShort))

    Scenario("add with carry"):
      Given("two different (large) positive numbers")
      When("added")
      Then("result is sum trimmed to 16b and carry flag is set")
      //NOTE: forAll may sometimes not respect Gen min/max values due to defect in ScalaCheck
      forAll(largePositiveValueGen, largePositiveValueGen):
        (a, b) => assert(Alu(a,b,0xFF00.toShort,AluOp.Add) == ((a+b).toShort,0xFF04.toShort))

    Scenario("add negative numbers w/o borrow"):
      Given("two different (small) negative numbers")
      When("added")
      Then("result is (negative) sum trimmed to 16b and flags are off")
      forAll(smallPositiveValueGen, smallPositiveValueGen):
        (a, b) => assert(Alu((-a).toShort, (-b).toShort, 0xFF00.toShort, AluOp.Add) == ((-(a + b)).toShort, 0xFF00.toShort))

    Scenario("add negative numbers with borrow"):
      Given("two different (large) negative numbers")
      When("added")
      Then("result is sum trimmed to (positive) 16b and borrow flag is set")
      forAll(largePositiveValueGen, largePositiveValueGen):
        (a, b) => assert(Alu((-a).toShort, (-b).toShort, 0xFF00.toShort, AluOp.Add) == ((-(a + b)).toShort, 0xFF02.toShort))

    Scenario("add large numbers with opposite signs"):
      Given("two different (large) numbers with opposite signs")
      When("added")
      Then("result is sum trimmed to 16b and flags are off")
      forAll(largePositiveValueGen, largePositiveValueGen):
        (a, b) => assert(Alu(a, (-b).toShort, 0xFF00.toShort, AluOp.Add) == ((a - b).toShort, 0xFF00.toShort))

    Scenario("express Sub as Add with second operand negated"):
      Given("two different numbers")
      When("subtracted")
      Then("result is the same as sum with second operand negated")
      forAll(anyValueGen, anyValueGen):
        (a, b) => assert(Alu(a, b, 0xFF00.toShort, AluOp.Sub) == Alu(a, (-b).toShort, 0xFF00.toShort, AluOp.Add))

    Scenario("bitwise And"):
      Given("two different numbers")
      When("and'ed")
      Then("result is bitwise And")
      forAll(anyValueGen,anyValueGen):
        (a, b) =>
          assert(Alu(a, b, 0xFFFF.toShort, AluOp.And) == ((a & b).toShort, (0xFFFE | (if ((a & b)==0) 1 else 0)).toShort))

    Scenario("bitwise And with 0"):
      Given("any number")
      When("and'ed with 0")
      Then("result is 0 and zero flag is set")
      forAll(anyValueGen):
        a => assert(Alu(a, 0, 0xFFFF.toShort, AluOp.And) == (0, 0xFFFF.toShort))

    Scenario("bitwise Or"):
      Given("two different numbers")
      When("or'ed")
      Then("result is bitwise Or")
      forAll(anyValueGen, anyValueGen):
        (a, b) => whenever(a!=0 || b!=0):
          assert(Alu(a, b, 0xFFFF.toShort, AluOp.Or) == ((a | b).toShort, 0xFFFE.toShort))

    Scenario("bitwise Or of two 0s"):
      Given("zero as both operands")
      When("or'ed")
      Then("result is 0 and zero flag is set")
      assert(Alu(0, 0, 0xFFFF.toShort, AluOp.Or) == (0, 0xFFFF.toShort))

    Scenario("bitwise Xor"):
      Given("two different numbers")
      When("xor'ed")
      Then("result is bitwise Xor")
      forAll(anyValueGen, anyValueGen):
        (a, b) => whenever(a != b):
          assert(Alu(a, b, 0xFFFF.toShort, AluOp.Xor) == ((a ^ b).toShort, 0xFFFE.toShort))

    Scenario("bitwise Xor or same numbers"):
      Given("any same numbers")
      When("xor'ed")
      Then("result is 0 and zero flag is set")
      forAll(anyValueGen):
        a => assert(Alu(a, a, 0xFFFF.toShort, AluOp.Xor) == (0, 0xFFFF.toShort))

    Scenario("compare different numbers"):
      Given("two different numbers")
      When("compared")
      Then("zero flag is not set")
      forAll(anyValueGen, anyValueGen):
        (a, b) => whenever(a != b):
            assert(Alu(a, b, 0xFFFF.toShort, AluOp.Compare) == (0, 0xFFFE.toShort))

    Scenario("compare same numbers"):
      Given("two same numbers")
      When("compared")
      Then("zero flag is set")
      forAll(anyValueGen):
        a => assert(Alu(a, a, 0xFFFF.toShort, AluOp.Compare) == (1, 0xFFFF.toShort))

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

  private def createRandomStateCpu:Cpu =
    val cpu = (1 to 1000).foldLeft(testCpuHandler.create)((cpu, _) =>
      (for
        index <- registerIndexGen.sample
        value <- valueGen.sample
      yield cpu.setReg(index, value))
        .getOrElse(cpu)
    )
    // Make sure that generators actually worked (it is practically impossible not to set 3+ register in 1000 takes)
    assert((0 to 15).count(cpu.register(_)!=0) > 3)
    cpu

  private def generateMemoryContents:Map[Int,Short] =
    val pairs = (1 to 1000).foldLeft(Map[Int,Short]())((map, _) =>
      (for
        address <- addressGen.sample
        value <- valueGen.sample
      yield map + (address -> value))
        .getOrElse(map)
    )
    // Make sure that generators actually worked (it is practically impossible not to set 10+ addresses in 1000 takes)
    assert(pairs.keys.size>10)
    pairs
