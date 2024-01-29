package org.kr.cpu

import AluOp.Add

@main
def main(args: String*): Unit =
  println("Main CPU function doing nothing")

case class Cpu(handler:CpuHandler,register:Register,memory:Memory):
  def reset: Cpu = handler.reset(this)
  // predefined registers
  def pc: Short = register(0)
  def sp: Short = register(1)
  def fl: Short = register(2)
  def flZ: Boolean = (fl & 0x0001) == 1
  def a: Short = register(3)
  def setPc(value:Short): Cpu = setReg(0,value)
  def setSp(value:Short): Cpu = setReg(1,value)
  def setFl(value:Short): Cpu = setReg(2,value)
  def setAL(value:Short): Cpu = setReg(3,((a & 0xFF00) | (value & 0x00FF)).toShort)
  def setAH(value:Short): Cpu = setReg(3,((a & 0x00FF) | ((value & 0x00FF) << 8)).toShort)
  def setZ(): Cpu = setFl((fl | 0x0001).toShort)
  def clearZ(): Cpu = setFl((fl & 0xFFFE).toShort)

  def setReg(index:Int, value:Short): Cpu = handler.setReg(this, index,value)
  def incPC: Cpu = setPc((pc + 1).toShort)
  def incSP: Cpu = setSp((sp + 1).toShort)
  def decSP: Cpu = setSp((sp - 1).toShort)
  def writeMemory(address:Int, value: Short): Cpu = handler.writeMemory(this, address,value)
  def handleNext:Cpu = handler.handle(this, Instruction(memory(pc)))

trait CpuHandler:
  def create: Cpu
  def reset(cpu: Cpu): Cpu
  def setReg(cpu: Cpu, index: Int, value: Short): Cpu
  def writeMemory(cpu:Cpu, address: Int, value: Short): Cpu
  def handle(cpu:Cpu, instr:Instruction): Cpu

object CpuHandlerImmutable extends CpuHandler:
  override def create: Cpu = Cpu(CpuHandlerImmutable, emptyRegs, emptyMemory)
  override def reset(cpu: Cpu): Cpu = cpu.copy(register = emptyRegs)
  override def setReg(cpu: Cpu, index: Int, value: Short): Cpu = cpu.copy(register = cpu.register.set(index, value))
  override def writeMemory(cpu: Cpu, address: Int, value: Short): Cpu = cpu.copy(memory = cpu.memory.write(address,value))

  override def handle(cpu: Cpu, instr:Instruction): Cpu =
    (instr.opcode, cpu.flZ) match
      case (LD,_) => handleLD(cpu, instr)
      case (LDZ, true) | (LDNZ, false) => this.handleLD(cpu,instr) // handle actual LD instruction
      case (LDZ, false) | (LDNZ, true) => handleNOP(cpu) // do nothing (NOP)
      case (ADD, _) | (SUB, _) | (AND, _) | (OR, _) | (XOR, _) | (CMP, _) => handleALU(cpu,instr)
      case _ => throw new IllegalArgumentException(f"Illegal instruction: ${instr.value}%04X at ${cpu.pc}%04X")  //cpu.incPC

  private def handleNOP(cpu: Cpu):Cpu = cpu.incPC

  private def handleLD(cpu: Cpu, instr:Instruction): Cpu =
    instr.mode match
      case NOP_MODE => handleNOP(cpu)
      case IMMEDIATE_LOW => cpu.setAL(instr.immediate).incPC
      case IMMEDIATE_HIGH => cpu.setAH(instr.immediate).incPC
      case REGISTERS =>
        val handled = cpu.setReg(instr.reg2, cpu.register(instr.reg1))
        //NOTE: do not increase PC if r2 is PC (0)
        if (instr.reg2 != 0) handled.incPC else handled
      case MEMORY2REG =>
        val handled = cpu.setReg(instr.reg2, cpu.memory(cpu.register(instr.reg1)))
        //NOTE: do not increase PC if r2 is PC (0)
        if (instr.reg2 != 0) handled.incPC else handled
      case REG2MEMORY => cpu.writeMemory(cpu.register(instr.reg2), cpu.register(instr.reg1)).incPC
      case _ => throw new IllegalArgumentException(f"Illegal instruction: ${instr.value}%04X at ${cpu.pc}%04X")  //cpu.incPC

  private def handleALU(cpu: Cpu, instr: Instruction): Cpu =
    val oper = instr.opcode match
      case ADD => AluOp.Add
      case SUB => AluOp.Sub
      case AND => AluOp.And
      case OR => AluOp.Or
      case XOR => AluOp.Xor
      case CMP => AluOp.Compare
      case _ => throw new IllegalArgumentException(f"Illegal instruction: ${instr.value}%04X at ${cpu.pc}%04X")

    val res = Alu(cpu.register(instr.reg1),cpu.register(instr.reg2),cpu.fl,oper)
    cpu.setReg(instr.reg1, res._1).setFl(res._2).incPC

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
  override def write(address: Int, value: Short): Memory =
    val actualAddress = if(address<0) address+0x10000 else address
    copy(m = m.updated(actualAddress, value))

enum AluOp:
  case Add, Sub, And, Or, Xor, Compare

object Alu:
  def apply(a:Short,b:Short,f:Short,op:AluOp):(Short,Short) =
    op match
      case AluOp.Add => add(a,b,f)
      case AluOp.Sub => add(a,(-b).toShort,f)
      case AluOp.And | AluOp.Or | AluOp.Xor => bitwise(a,b,f,op)
      case AluOp.Compare => compare(a,b,f)

  private def add(a: Short, b: Short, f: Short):(Short,Short) =
    val result = (a + b).toShort
    val carry = (a > 0 && b > 0) && result < 0
    val borrow = (a < 0 && b < 0) && result > 0
    val zero = result == 0
    val newF = f & 0xFFF8 | (bool2bit(carry) << 2) | (bool2bit(borrow) << 1) | bool2bit(zero)
    //println(f"a $a b $b r $result f $newF $newF%04X")
    (result, newF.toShort)

  private def bitwise(a: Short, b: Short, f: Short, op:AluOp):(Short,Short) =
    val result = op match
      case AluOp.And => a & b
      case AluOp.Or => a | b
      case AluOp.Xor => a ^ b
      case _ => throw IllegalArgumentException(f"bitwise operation not supported: $op")

    val zero = result == 0
    val newF = f & 0xFFFE | bool2bit(zero)
    //println(f"a $a b $b r $result f $newF $newF%04X op: $op")
    (result.toShort, newF.toShort)

  private def compare(a: Short, b: Short, f: Short): (Short, Short) =
    val result = if(a==b) 1 else 0
    val zero = result == 1
    val newF = f & 0xFFFE | bool2bit(zero)
    //println(f"a $a b $b r $result f $newF $newF%04X")
    (a, newF.toShort)

  private def bool2bit(bool:Boolean):Int = if (bool) 1 else 0