package org.kr.cpu
package test

import org.scalacheck.Gen

object TestUtils:
  // NOTE: forAll after first failed test may ignore conditions in generator
  // Always verify the first failed test
  val smallPositiveValueGen: Gen[Short] = Gen.choose(1.toShort, 0x3FFF.toShort)
  val largePositiveValueGen: Gen[Short] = Gen.choose(0x4000.toShort, 0x7FFF.toShort)
  val anyValueGen: Gen[Short] = Gen.choose(Short.MinValue, Short.MaxValue)
  val byteValueGen: Gen[Short] = Gen.choose(0.toShort, 0xFF.toShort)
  val registerIndexGen: Gen[Short] = Gen.choose(0.toShort, 15.toShort)
  val addressGen: Gen[Int] = Gen.choose(0, 0xFFFF)

  val testCpuHandler:CpuHandler = CpuHandlerImmutable

  def createRandomStateCpu: Cpu =
    val cpu = (1 to 1000).foldLeft(testCpuHandler.create)((cpu, _) =>
      (for
        index <- registerIndexGen.sample
        value <- anyValueGen.sample
      yield cpu.setReg(index, value))
        .getOrElse(cpu)
    )
    // Make sure that generators actually worked (it is practically impossible not to set 3+ register in 1000 takes)
    assert((0 to 15).count(cpu.register(_) != 0) > 3)
    cpu
  
  def createRandomStateCpuWithMemory(toFill:Int = 50000):Cpu =
    generateMemoryContents(toFill).foldLeft(createRandomStateCpu)({ case (cpu, pair) => cpu.writeMemory(pair._1, pair._2) })


  def generateMemoryContents(toFill:Int = 1000): Map[Int, Short] =
    val pairs = (1 to toFill).foldLeft(Map[Int, Short]())((map, _) =>
      (for
        address <- addressGen.sample
        value <- anyValueGen.sample
      yield map + (address -> value))
        .getOrElse(map)
    )
    // Make sure that generators actually worked (it is practically impossible not to set 10+ addresses in 1000 takes)
    assert(pairs.keys.size > 10)
    pairs
    
  def createResetCpu: Cpu = testCpuHandler.create.reset
