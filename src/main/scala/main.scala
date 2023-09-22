package org.kr.cpu

@main
def main(): Unit =
  println("Hello world!")

case class Cpu(handler:CpuHandler,register:Register):
  def reset: Cpu = handler.reset(this)
  // predefined registers
  def pc: Short = register(0)
  def sp: Short = register(1)
  def fl: Short = register(2)
  def a: Short = register(3)
  def setPc(value:Short): Cpu = setReg(0,value)
  def setSp(value:Short): Cpu = setReg(1,value)
  def setReg(index:Int, value:Short): Cpu = handler.setReg(this, index,value)
  def incPC: Cpu = setPc((pc + 1).toShort)
  def incSP: Cpu = setSp((sp + 1).toShort)
  def decSP: Cpu = setSp((sp - 1).toShort)

trait CpuHandler:
  def create: Cpu
  def reset(cpu: Cpu): Cpu
  def setReg(cpu: Cpu, index:Int,value:Short): Cpu

object CpuHandlerImmutable extends CpuHandler:
  override def create: Cpu = Cpu(CpuHandlerImmutable, emptyRegs)
  override def reset(cpu: Cpu): Cpu = cpu.copy(register = emptyRegs)
  override def setReg(cpu: Cpu, index: Int, value: Short): Cpu = cpu.copy(register = cpu.register.set(index, value))


  private def emptyRegs: Register = RegisterImmutable.empty

trait Register:
  def apply(index:Int):Short
  def set(index:Int, value:Short):Register

case class RegisterImmutable(r:Vector[Short]) extends Register:
  override def apply(index: Int): Short = r(index)
  override def set(index:Int, value:Short):Register = copy(r = r.updated(index,value))

  override def toString: String = r.indices.map(i=>f"$i%01X:0x${r(i)}%04X").mkString("|")

object RegisterImmutable:
  def empty: Register = new RegisterImmutable(Vector.fill(16)(0))