package org.kr.cpu

sealed trait Opcode:
  def code: Int
  def isLegal: Boolean = true
  def isAlu: Boolean = false

case object LD extends Opcode {override val code:Int=0x0 }
case object LDZ extends Opcode {override val code:Int=0x6 }
case object LDNZ extends Opcode {override val code:Int=0x7 }

case object ADD extends Opcode {override val code:Int=0x8; override val isAlu:Boolean = true}
case object SUB extends Opcode {override val code:Int=0x9; override val isAlu:Boolean = true}
case object INC extends Opcode {override val code:Int=0xA; override val isAlu:Boolean = true}
case object DEC extends Opcode {override val code:Int=0xB; override val isAlu:Boolean = true}
case object CMP extends Opcode {override val code:Int=0xC; override val isAlu:Boolean = true}
case object AND extends Opcode {override val code:Int=0xD; override val isAlu:Boolean = true}
case object OR extends Opcode {override val code:Int=0xE; override val isAlu:Boolean = true}
case object XOR extends Opcode {override val code:Int=0xF; override val isAlu:Boolean = true}

case class OpcodeIllegal(override val code:Int) extends Opcode:
  override val isLegal: Boolean = false

object Opcode:
  val codes:Vector[Opcode]=Vector(LD,OpcodeIllegal(1),OpcodeIllegal(2),OpcodeIllegal(3),OpcodeIllegal(4),OpcodeIllegal(5),LDZ,LDNZ,ADD,SUB,INC,DEC,CMP,AND,OR,XOR)

sealed trait AddressMode:
  def code: Int
  def isLegal: Boolean = true

case object NOP_MODE extends AddressMode {override val code:Int=0 }
case object IMMEDIATE_LOW extends AddressMode {override val code:Int=0x8 }
case object IMMEDIATE_HIGH extends AddressMode {override val code:Int=0x9 }
case object REGISTERS extends AddressMode {override val code:Int=0x3 }
case object REG2MEMORY extends AddressMode {override val code:Int=0x4 }
case object MEMORY2REG extends AddressMode {override val code:Int=0x5 }
case object OUTPUT_REG extends AddressMode {override val code:Int=0x6 }
case object INPUT_REG extends AddressMode {override val code:Int=0x7 }

case class AddressModeIllegal(override val code:Int) extends AddressMode:
  override val isLegal: Boolean = false

object AddressMode:
  val codes:Vector[AddressMode]=Vector(NOP_MODE,AddressModeIllegal(1),AddressModeIllegal(2),REGISTERS,
    REG2MEMORY,MEMORY2REG,OUTPUT_REG,INPUT_REG,IMMEDIATE_LOW,IMMEDIATE_HIGH,AddressModeIllegal(0xA),
    AddressModeIllegal(0xB),AddressModeIllegal(0xC),AddressModeIllegal(0xD),AddressModeIllegal(0xE),AddressModeIllegal(0xF))

class Instruction(val value:Short):
  // bits 0-3
  val opcode:Opcode = Opcode.codes((value & 0x000F).toShort)
  // bits 4-7
  val mode:AddressMode = AddressMode.codes(((value & 0x00F0) >> 4).toShort)
  // bits 8-15 depending on address mode (immediate)
  lazy val immediate: Short = ((value & 0xFF00) >> 8).toShort
  // bits 8-11 depending on address mode (register/memory)
  private lazy val reg1: Short = ((value & 0x0F00) >> 8).toShort
  lazy val regSrc: Short = reg1 // register-register
  lazy val regResult: Short = reg1 // ALU
  lazy val reg: Short = reg1 // register-memory, in-out
  // bits 12-15 depending on address mode (register/memory, in/out)
  private lazy val reg2: Short = ((value & 0xF000) >> 12).toShort
  lazy val regDest: Short = reg2 // register-register
  lazy val regOperand: Short = reg2 // ALU
  lazy val port: Short = reg2 // in/out
  lazy val addr: Short = reg2 // register-memory

  def valueWithOpcode(newOpcode:Opcode): Short = (value & 0xFFF0 | newOpcode.code).toShort
  def replaceOpcode(newOpcode:Opcode): Instruction = Instruction(valueWithOpcode(newOpcode))

case class INSTR_NOP() extends Instruction((LD.code | (NOP_MODE.code << 4)).toShort)
case class INSTR_LD_AL(imm:Short) extends Instruction((LD.code | (IMMEDIATE_LOW.code << 4) | ((imm & 0xFF) << 8)).toShort)
case class INSTR_LD_AH(imm:Short) extends Instruction((LD.code | (IMMEDIATE_HIGH.code << 4) | ((imm & 0xFF) << 8)).toShort)
case class INSTR_LD_RR(r1:Short,r2:Short) extends Instruction((LD.code | (REGISTERS.code << 4) | ((r1 & 0x000F) << 8) | ((r2 & 0x000F) << 12)).toShort)
case class INSTR_JMP_A() extends Instruction((LD.code | (REGISTERS.code << 4) | (3 << 8)).toShort) // helper instruction for LD A => PC
case class INSTR_LD_MR(r:Short,a:Short) extends Instruction((LD.code | (MEMORY2REG.code << 4) | ((r & 0x000F) << 8) | ((a & 0x000F) << 12)).toShort)
case class INSTR_LD_RM(r:Short,a:Short) extends Instruction((LD.code | (REG2MEMORY.code << 4) | ((r & 0x000F) << 8) | ((a & 0x000F) << 12)).toShort)
case class INSTR_LD_RO(r: Short, p: Short) extends Instruction((LD.code | (OUTPUT_REG.code << 4) | ((r & 0x000F) << 8) | ((p & 0x000F) << 12)).toShort)
case class INSTR_LD_RI(r: Short, p: Short) extends Instruction((LD.code | (INPUT_REG.code << 4) | ((r & 0x000F) << 8) | ((p & 0x000F) << 12)).toShort)

case class INSTR_LDZ_AL(imm:Short) extends Instruction(INSTR_LD_AL(imm).valueWithOpcode(LDZ))
case class INSTR_LDZ_AH(imm:Short) extends Instruction(INSTR_LD_AH(imm).valueWithOpcode(LDZ))
case class INSTR_LDZ_RR(r1:Short,r2:Short) extends Instruction(INSTR_LD_RR(r1,r2).valueWithOpcode(LDZ))
case class INSTR_JMPZ_A() extends Instruction(INSTR_JMP_A().valueWithOpcode(LDZ)) // helper instruction for LDZ A => PC
case class INSTR_LDZ_MR(r:Short,a:Short) extends Instruction(INSTR_LD_MR(r,a).valueWithOpcode(LDZ))
case class INSTR_LDZ_RM(r:Short,a:Short) extends Instruction(INSTR_LD_RM(r,a).valueWithOpcode(LDZ))
case class INSTR_LDZ_RO(r: Short, p: Short) extends Instruction(INSTR_LD_RO(r,p).valueWithOpcode(LDZ))
case class INSTR_LDZ_RI(r: Short, p: Short) extends Instruction(INSTR_LD_RI(r,p).valueWithOpcode(LDZ))

case class INSTR_LDNZ_AL(imm:Short) extends Instruction(INSTR_LD_AL(imm).valueWithOpcode(LDNZ))
case class INSTR_LDNZ_AH(imm:Short) extends Instruction(INSTR_LD_AH(imm).valueWithOpcode(LDNZ))
case class INSTR_LDNZ_RR(r1:Short,r2:Short) extends Instruction(INSTR_LD_RR(r1,r2).valueWithOpcode(LDNZ))
case class INSTR_JMPNZ_A() extends Instruction(INSTR_JMP_A().valueWithOpcode(LDNZ)) // helper instruction for LDNZ A => PC
case class INSTR_LDNZ_MR(r:Short,a:Short) extends Instruction(INSTR_LD_MR(r,a).valueWithOpcode(LDNZ))
case class INSTR_LDNZ_RM(r:Short,a:Short) extends Instruction(INSTR_LD_RM(r,a).valueWithOpcode(LDNZ))
case class INSTR_LDNZ_RO(r: Short, p: Short) extends Instruction(INSTR_LD_RO(r, p).valueWithOpcode(LDNZ))
case class INSTR_LDNZ_RI(r: Short, p: Short) extends Instruction(INSTR_LD_RI(r, p).valueWithOpcode(LDNZ))

class INSTR_ALU(code:Opcode,r1:Short,r2:Short) extends Instruction((code.code | (REGISTERS.code << 4) | ((r1 & 0x000F) << 8) | ((r2 & 0x000F) << 12)).toShort) 

case class INSTR_ADD(r1:Short,r2:Short) extends INSTR_ALU(ADD,r1,r2)
case class INSTR_SUB(r1:Short,r2:Short) extends INSTR_ALU(SUB,r1,r2)
case class INSTR_INC(r1:Short) extends INSTR_ALU(INC,r1,0)
case class INSTR_DEC(r1:Short) extends INSTR_ALU(DEC,r1,0)
case class INSTR_INC_SP() extends INSTR_ALU(INC,1,0) // helper instruction
case class INSTR_DEC_SP() extends INSTR_ALU(DEC,1,0) // helper instruction
case class INSTR_CMP(r1:Short,r2:Short) extends INSTR_ALU(CMP,r1,r2)
case class INSTR_AND(r1:Short,r2:Short) extends INSTR_ALU(AND,r1,r2)
case class INSTR_OR(r1:Short,r2:Short) extends INSTR_ALU(OR,r1,r2)
case class INSTR_XOR(r1:Short,r2:Short) extends INSTR_ALU(XOR,r1,r2)

// helper macros
object MACRO:
  def LD_A(imm: Short): Vector[Short] = // 2 steps
    Vector(INSTR_LD_AL((imm & 0x00FF).toShort).value,INSTR_LD_AH(((imm >> 8) & 0x00FF).toShort).value)
  def LD_R(imm: Short, reg: Short): Vector[Short] = // 3 steps
    LD_A(imm) :+ INSTR_LD_RR(3,reg).value
  def RET: Vector[Short] = // 3 steps
    Vector(INSTR_LD_MR(3, 1).value, // LD (SP) => A - pop return address from stack
      INSTR_INC_SP().value,
      INSTR_JMP_A().value) // LD A => PC - jump to return address
  def CALL(baseAddress: Short, callAddress: Short): Vector[Short] = // 7 steps
    Vector(INSTR_DEC_SP().value) ++ // DEC SP
      MACRO.LD_A((baseAddress + 7).toShort) ++ // return address to A (determined at compile time)
      Vector(INSTR_LD_RM(3, 1).value) ++ // LD A => (SP) - push return address to stack
      MACRO.LD_A(callAddress) ++ // call address
      Vector(INSTR_JMP_A().value) // LD A => PC - jump to subroutine
  def JMPI(targetAddress: Short): Vector[Short] = // 3 steps
    LD_A(targetAddress) ++ Vector(INSTR_JMP_A().value)
  def JMPIZ(targetAddress: Short): Vector[Short] = // 3 steps
    LD_A(targetAddress) ++ Vector(INSTR_JMPZ_A().value)
  def JMPINZ(targetAddress: Short): Vector[Short] = // 3 steps
    LD_A(targetAddress) ++ Vector(INSTR_JMPNZ_A().value)