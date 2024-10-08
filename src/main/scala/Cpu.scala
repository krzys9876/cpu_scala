package org.kr.cpu

import scala.annotation.tailrec

case class Cpu(handler:CpuHandler, register:Register, memory:Memory, outputFile:OutputFile, inputFile: InputFile):
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
  def writeMemoryMulti(address:Int, values: Vector[Short]): Cpu =
    values.indices.foldLeft(this)((cpu, offset)=>cpu.writeMemory(address+offset, values(offset)))
  def output(port:Short, value: Short): Cpu = handler.output(this, port, value)
  def input(port:Short, reg: Short): Cpu = handler.input(this, port, reg)
  def handleNext:Cpu = handler.handle(this, Instruction(memory(pc)))
  @tailrec
  final def handleNext(steps:Long, waitEvery: Long = 0):Cpu =
    assume(steps >= 0)
    if((waitEvery>0) && (steps % waitEvery == 0)) Thread.sleep(1)
    steps match
      case 0 => this
      case _ => handleNext.handleNext(steps-1, waitEvery)
  @tailrec
  final def handleInfinitely:Cpu =
    handleNext.handleInfinitely

trait CpuHandler:
  def create: Cpu
  def reset(cpu: Cpu): Cpu
  def setReg(cpu: Cpu, index: Int, value: Short): Cpu
  def writeMemory(cpu:Cpu, address: Int, value: Short): Cpu
  def handle(cpu:Cpu, instr:Instruction): Cpu
  def output(cpu:Cpu, port: Short, value: Short): Cpu
  def input(cpu: Cpu, port: Short, reg: Short): Cpu

object CpuHandlerImmutable extends CpuHandler:
  override def create: Cpu = Cpu(CpuHandlerImmutable, emptyRegs, emptyMemory, OutputFile.blank, InputFile.blank)
  override def reset(cpu: Cpu): Cpu = cpu.copy(register = emptyRegs)
  override def setReg(cpu: Cpu, index: Int, value: Short): Cpu = cpu.copy(register = cpu.register.set(index, value))
  override def writeMemory(cpu: Cpu, address: Int, value: Short): Cpu = cpu.copy(memory = cpu.memory.write(address,value))
  override def output(cpu: Cpu, port: Short, value: Short): Cpu = cpu.copy(outputFile = cpu.outputFile.write(port, value))
  override def input(cpu: Cpu, port: Short, reg: Short): Cpu =
    val readValue = cpu.inputFile.read(port)
    cpu.setReg(reg, readValue._1).copy(inputFile = readValue._2)

  override def handle(cpu: Cpu, instr:Instruction): Cpu =
    (instr.opcode, cpu.flZ) match
      case (op, _) if op.isAlu => handleALU(cpu,instr)
      case (LD,_) => handleLD(cpu, instr)
      case (LDZ, true) | (LDNZ, false) => handleLD(cpu,instr) // handle actual LD instruction
      case (LDZ, false) | (LDNZ, true) => handleNOP(cpu) // do nothing (NOP)
      case _ => throw new IllegalArgumentException(f"Illegal instruction: ${instr.value}%04X at ${cpu.pc}%04X")  //cpu.incPC

  private def handleNOP(cpu: Cpu):Cpu = cpu.incPC

  private def handleLD(cpu: Cpu, instr:Instruction): Cpu =
    instr.mode match
      case NOP_MODE => handleNOP(cpu)
      case IMMEDIATE_LOW => cpu.setAL(instr.immediate).incPC
      case IMMEDIATE_HIGH => cpu.setAH(instr.immediate).incPC
      case REGISTERS => // rSrc=>rDest
        val handled = cpu.setReg(instr.regDest, cpu.register(instr.regSrc))
        //NOTE: do not increase PC if r2 is PC (0)
        if (instr.regDest != 0) handled.incPC else handled
      case MEMORY2REG => // reg<=(addr)
        val handled = cpu.setReg(instr.reg, cpu.memory(cpu.register(instr.addr)))
        //NOTE: do not increase PC if r2 is PC (0)
        if (instr.reg != 0) handled.incPC else handled
      case REG2MEMORY => // reg=>(addr)
        cpu.writeMemory(cpu.register(instr.addr), cpu.register(instr.reg)).incPC
      case OUTPUT_REG => // reg=>port
        cpu.output(instr.port, cpu.register(instr.reg)).incPC
      case _ => throw new IllegalArgumentException(f"Illegal LD instruction: ${instr.value}%04X at ${cpu.pc}%04X")  //cpu.incPC

  private def handleALU(cpu: Cpu, instr: Instruction): Cpu =
    val res = Alu(cpu.register(instr.regResult),cpu.register(instr.regOperand),cpu.fl,instr.opcode)
    cpu.setReg(instr.regResult, res._1).setFl(res._2).incPC

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
  case Shl, Shr, Flb, Add, Sub, Inc, Dec, And, Or, Xor, Compare

object Alu:
  def apply(a:Short,b:Short,f:Short,code:Opcode):(Short,Short) =
    val oper = code match
      case SHL => AluOp.Shl
      case SHR => AluOp.Shr
      case FLB => AluOp.Flb
      case ADD => AluOp.Add
      case SUB => AluOp.Sub
      case INC => AluOp.Inc
      case DEC => AluOp.Dec
      case AND => AluOp.And
      case OR => AluOp.Or
      case XOR => AluOp.Xor
      case CMP => AluOp.Compare
      case _ => throw new IllegalArgumentException(f"Illegal ALU instruction: ${code.code}%04X")
    calculate(a,b,f,oper)

  private def calculate(a:Short,b:Short,f:Short,op:AluOp):(Short,Short) =
    op match
      case AluOp.Shl | AluOp.Shr => shift(b,f,op)
      case AluOp.Flb => flipBytes(b,f)
      case AluOp.Add => add(a,b,f)
      case AluOp.Sub => add(a,(-b).toShort,f)
      case AluOp.Inc => add(a,1,f) // ignore b
      case AluOp.Dec => add(a,-1,f) // ignore b
      case AluOp.And | AluOp.Or | AluOp.Xor => bitwise(a,b,f,op)
      case AluOp.Compare => compare(a,b,f)

  private def toUnsigned(n: Short): Int = if(n<0) n+65536 else n

  private def add(a: Short, b: Short, f: Short):(Short,Short) =
    val result = (a + b).toShort
    //NOTE: carry flag may be treated as reversed borrow when using signed negative numbers
    // see: https://en.wikipedia.org/wiki/Carry_flag#Vs._borrow_flag
    val carry = (toUnsigned(a)+toUnsigned(b)) > 65535 // (a > 0 && b > 0) && result < 0
    val newF = composeFlags(f, carry, result)
    //println(f"a $a b $b r $result f $newF $newF%04X")
    (result, newF)

  private def bitwise(a: Short, b: Short, f: Short, op:AluOp):(Short,Short) =
    val result = (op match
      case AluOp.And => a & b
      case AluOp.Or => a | b
      case AluOp.Xor => a ^ b
      case _ => throw IllegalArgumentException(f"bitwise operation not supported: $op")
      ).toShort

    val newF = composeFlags(f, result)
    //println(f"a $a b $b r $result f $newF $newF%04X op: $op")
    (result, newF)

  private def compare(a: Short, b: Short, f: Short): (Short, Short) =
    val result = (if(a==b) 0 else 1).toShort // 0 means: set ZERO flag, which is what we want here
    val newF = composeFlags(f, result)
    //println(f"a $a b $b r $result f $newF $newF%04X")
    (a, newF)

  private def shift(b: Short, f: Short, op:AluOp):(Short,Short) =
    val (result, carry) = op match
      case AluOp.Shl => ((b << 1).toShort, (b & 0x8000)>0)
      case AluOp.Shr => ((b >> 1).toShort, (b & 0x8001)>0)
      case _ => throw IllegalArgumentException(f"shift operation not supported: $op")

    val newF = composeFlags(f, carry, result)
    //println(f"b $b%04X r $result%04X f $newF $newF%04X")
    (result, newF)

  private def flipBytes(b: Short, f: Short): (Short, Short) =
    val result = (((b & 0x00FF) << 8) | ((b & 0xFF00) >> 8)).toShort
    val newF = composeFlags(f, result)
    //println(f"b $b%04X r $result%04X f $newF $newF%04X")
    (result, newF)

  private def bool2bit(bool:Boolean):Int = if (bool) 1 else 0
  private def result2zeroFlag(result:Short):Int = if (result==0) 1 else 0
  private def composeFlags(f: Short, carry: Boolean, result: Short): Short =
    (f & 0xFFF8 | (bool2bit(carry) << 1) | result2zeroFlag(result)).toShort
  private def composeFlags(f: Short, result: Short): Short =
    (f & 0xFFFE | result2zeroFlag(result)).toShort
