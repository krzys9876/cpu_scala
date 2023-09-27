package org.kr.cpu

sealed trait Opcode:
  def code: Int
  def isLegal: Boolean = true

case object LD extends Opcode {override val code:Int=0x0 }
case object RET extends Opcode {override val code:Int=0x3 }
case object JR extends Opcode {override val code:Int=0x4 }
case object CALL extends Opcode {override val code:Int=0x5 }
case object JPZ extends Opcode {override val code:Int=0x6 }
case object JPNZ extends Opcode {override val code:Int=0x7 }

case object ADD extends Opcode {override val code:Int=0x8 }
case object SUB extends Opcode {override val code:Int=0xA }
case object CMP extends Opcode {override val code:Int=0xB }
case object AND extends Opcode {override val code:Int=0xC }
case object OR extends Opcode {override val code:Int=0xD }
case object XOR extends Opcode {override val code:Int=0xE }

case class OpcodeIllegal(override val code:Int) extends Opcode:
  override val isLegal: Boolean = false

object Opcode:
  val codes:Vector[Opcode]=Vector(LD,OpcodeIllegal(1),OpcodeIllegal(2),RET,JR,CALL,JPZ,JPNZ,ADD,OpcodeIllegal(9),SUB,CMP,AND,OR,XOR,OpcodeIllegal(0xF))

sealed trait AddressMode:
  def code: Int
  def isLegal: Boolean = true

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
  val codes:Vector[AddressMode]=Vector(AddressModeIllegal(0),AddressModeIllegal(1),AddressModeIllegal(2),REGISTERS,
    REG2MEMORY,MEMORY2REG,OUTPUT_REG,INPUT_REG,IMMEDIATE_LOW,IMMEDIATE_HIGH,AddressModeIllegal(0xA),
    AddressModeIllegal(0xB),AddressModeIllegal(0xC),AddressModeIllegal(0xD),AddressModeIllegal(0xE),AddressModeIllegal(0xF))

case class Instruction(value:Short):
  // bits 0-3
  val opcode:Opcode = Opcode.codes((value & 0x000F).toShort)
  // bits 4-7
  val mode:AddressMode = AddressMode.codes(((value & 0x00F0) >> 4).toShort)
  // bits 8-15 depending on address mode
  lazy val immediate: Short = ((value & 0xFF00) >> 8).toShort
  // bits 8-11 depending on address mode
  lazy val reg1: Short = ((value & 0x0F00) >> 8).toShort
  // bits 12-15 depending on address mode
  lazy val reg2: Short = ((value & 0xF000) >> 12).toShort
