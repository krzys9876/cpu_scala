package org.kr.cpu

@main
def main(args: String*): Unit =
  println("Main CPU function doing nothing")

case class Cpu(handler:CpuHandler,register:Register,memory:Memory):
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
  def writeMemory(address:Int, value: Short): Cpu = handler.writeMemory(this, address,value)

trait CpuHandler:
  def create: Cpu
  def reset(cpu: Cpu): Cpu
  def setReg(cpu: Cpu, index: Int, value: Short): Cpu
  def writeMemory(cpu:Cpu, address: Int, value: Short): Cpu

object CpuHandlerImmutable extends CpuHandler:
  override def create: Cpu = Cpu(CpuHandlerImmutable, emptyRegs, emptyMemory)
  override def reset(cpu: Cpu): Cpu = cpu.copy(register = emptyRegs)
  override def setReg(cpu: Cpu, index: Int, value: Short): Cpu = cpu.copy(register = cpu.register.set(index, value))
  override def writeMemory(cpu: Cpu, address: Int, value: Short): Cpu = cpu.copy(memory = cpu.memory.write(address,value))


  private def emptyRegs: Register = RegisterImmutable.empty
  private def emptyMemory: Memory = MemoryImmutable()

trait Register:
  def apply(index:Int):Short
  def set(index:Int, value:Short):Register

case class RegisterImmutable(r:Vector[Short]) extends Register:
  override def apply(index: Int): Short = r(index)
  override def set(index:Int, value:Short):Register = copy(r = r.updated(index,value))

  override def toString: String = r.indices.map(i=>f"$i%01X:0x${r(i)}%04X").mkString("|")

object RegisterImmutable:
  def empty: Register = new RegisterImmutable(Vector.fill(16)(0))

trait Memory:
  def apply(address:Int):Short
  def write(address:Int, value:Short):Memory

case class MemoryImmutable(m:Vector[Short] = Vector.fill[Short](0xFFFF+1)(0.toShort)) extends Memory:
  override def apply(address: Int): Short = m(address & 0xFFFF)
  override def write(address: Int, value: Short): Memory = copy(m = m.updated(address, value))

enum AluOp:
  case Add, Sub

object Alu:
  def apply(a:Short,b:Short,f:Short,op:AluOp):(Short,Short) =
    op match
      case AluOp.Add => add(a,b,f)
      case AluOp.Sub => add(a,(-b).toShort,f)

  private def add(a: Short, b: Short, f: Short):(Short,Short) =
    val result = (a + b).toShort
    val carry = (a > 0 && b > 0) && result < 0
    val borrow = (a < 0 && b < 0) && result > 0
    val zero = result == 0
    val newF = f & 0xFFF8 | (bool2bit(carry) << 2) | (bool2bit(borrow) << 1) | bool2bit(zero)
    println(f"a $a b $b r $result f $newF $newF%04X")
    (result, newF.toShort)

private def bool2bit(bool:Boolean):Int = if (bool) 1 else 0