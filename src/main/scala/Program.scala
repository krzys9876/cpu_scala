package org.kr.cpu

case class Program(instructions: Vector[InstructionLine]):
  private def init: Cpu =
    val cpuInit = CpuHandlerImmutable.create
    val cpuMemoryInit = instructions.foldLeft(cpuInit)((cpu, instr)=> cpu.writeMemory(instr.address, instr.instr.value))
    cpuMemoryInit.reset

  def run: Cpu = init.handleInfinitely
  def run(steps: Long, waitEvery: Long = 0): Cpu = init.handleNext(steps, waitEvery)

case class InstructionLine(address: Short, instr: Instruction)