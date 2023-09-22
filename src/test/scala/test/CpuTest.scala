package org.kr.cpu
package test

import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CpuTest extends AnyFeatureSpec with GivenWhenThen with ScalaCheckPropertyChecks:
  val testCpuHandler:CpuHandler = CpuHandlerImmutable

  val registerIndexGen:Gen[Short] = Gen.choose(0,15)
  val registerValueGen:Gen[Short] = Gen.choose(-0x8000,0x7FFF)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50, maxDiscardedFactor = 30.0)

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
      forAll(registerIndexGen,registerValueGen):
        (index, value) =>
          val cpuSet = cpuInit.setReg(index,value)
          cpuSet.register(index)==value

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


  private def createRandomStateCpu:Cpu =
    (1 to 100).foldLeft(testCpuHandler.create)({ case (cpu, _) =>
      (for
        index <- registerIndexGen.sample
        value <- registerValueGen.sample
      yield cpu.setReg(index, value))
        .getOrElse(cpu)
    })