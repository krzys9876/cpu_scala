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

case object ILLEGAL1 extends Opcode {override val code:Int=0x1; override val isLegal: Boolean = false}
case object ILLEGAL2 extends Opcode {override val code:Int=0x2; override val isLegal: Boolean = false }
case object ILLEGAL9 extends Opcode {override val code:Int=0x9; override val isLegal: Boolean = false }
case object ILLEGALF extends Opcode {override val code:Int=0xF; override val isLegal: Boolean = false }

object Opcode:
  val opcodes:Vector[Opcode]=Vector(LD,ILLEGAL1,ILLEGAL2,RET,JR,CALL,JPZ,JPNZ,ADD,ILLEGAL9,SUB,CMP,AND,OR,XOR,ILLEGALF)