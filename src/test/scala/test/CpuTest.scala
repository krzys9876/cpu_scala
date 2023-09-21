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
      println(cpuReset.register)
      Then("Cpu is in a initial state")
      forAll(registerIndexGen):
        index => assert(cpuReset.register(index) == 0)

  Feature("CPU register basic operations"):
    Scenario("set register value"):
      Given("a cpu instance in random state")
      val cpuInit = createRandomStateCpu
      println(cpuInit.register)
      When("a register is set to a given value")
      Then("the same value can be read from register")
      forAll(registerIndexGen,registerValueGen):
        (index, value) =>
          val cpuSet = cpuInit.setReg(index,value)
          cpuSet.register(index)==value


  private def createRandomStateCpu:Cpu =
    (1 to 100).foldLeft(testCpuHandler.create)({ case(cpu, _) =>
      (for {
        index <- registerIndexGen.sample
        value <- registerValueGen.sample
      } yield cpu.setReg(index,value))
        .getOrElse(cpu)
    })