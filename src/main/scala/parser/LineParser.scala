package org.kr.cpu
package parser

import scala.util.parsing.combinator.JavaTokenParsers

abstract class BaseParser[T] extends JavaTokenParsers:
  def result:Parser[T]

  def process(input: String): Either[String, T] =
    parseAll(result, input) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)

trait Operand:
  val num: Short

case class OpRegister(override val num: Short) extends Operand
case class OpMemory(override val num: Short) extends Operand
case class OpPort(override val num: Short) extends Operand
case class OpValue(override val num: Short) extends Operand


trait InstructionParser extends BaseParser[Instruction]:
  def parseNum(str:String): Short = str match
    case hex if hex.toLowerCase().startsWith("0x") => Integer.parseInt(str.substring(2),16).toShort
    case bin if bin.toLowerCase().startsWith("0b") => Integer.parseInt(str.substring(2), 2).toShort
    case dec => Integer.parseInt(str).toShort
    
  def parseReg(str:String): Short = Integer.parseInt(str.substring(1), 0x10).toShort

  private def operandPrefix = "[R|M|P]"
  private def operandSuffix = "[0-9|A-F]"
  private def operandHex = "0x[0-9|A-F]+"
  private def operandBin = "0b[0-1]+"
  private def operandDec = "-?[0-9]+"
  //NOTE: string is interpolated only to avoid compiler warning about "|" doubled characters in string
  private def operandText: Parser[String] = f"($operandPrefix$operandSuffix)".r
  private def operandNum: Parser[String] = operandHex.r | operandBin.r | operandDec.r  
  private def operand: Parser[Operand] = (operandText | operandNum) ^^ { op =>
    op.charAt(0) match
      case 'R' => OpRegister(parseReg(op))
      case 'M' => OpMemory(parseReg(op))
      case 'P' => OpPort(parseReg(op))
      case d if d.isDigit || d=='-' => OpValue(parseNum(op))} 

  private def decodeLDA(mnemonic: String, op: Operand): Instruction =
    (mnemonic, op) match
      case ("LDAL", v: OpValue) => INSTR_LD_AL(v.num)
      case ("LDALZ", v: OpValue) => INSTR_LDZ_AL(v.num)
      case ("LDALNZ", v: OpValue) => INSTR_LDNZ_AL(v.num)
      case ("LDAH", v: OpValue) => INSTR_LD_AH(v.num)
      case ("LDAHZ", v: OpValue) => INSTR_LDZ_AH(v.num)
      case ("LDAHNZ", v: OpValue) => INSTR_LDNZ_AH(v.num)

  private def decodeLD(mnemonic: String, op1: Operand, op2: Operand): Instruction =
    (mnemonic, op1, op2) match
      case ("LD", r1: OpRegister, r2: OpRegister) => INSTR_LD_RR(r1.num, r2.num)
      case ("LDZ", r1: OpRegister, r2: OpRegister) => INSTR_LDZ_RR(r1.num, r2.num)
      case ("LDNZ", r1: OpRegister, r2: OpRegister) => INSTR_LDNZ_RR(r1.num, r2.num)
      case ("LD", m1: OpMemory, r2: OpRegister) => INSTR_LD_MR(m1.num, r2.num)
      case ("LDZ", m1: OpMemory, r2: OpRegister) => INSTR_LDZ_MR(m1.num, r2.num)
      case ("LDNZ", m1: OpMemory, r2: OpRegister) => INSTR_LDNZ_MR(m1.num, r2.num)
      case ("LD", r1: OpRegister, m2: OpMemory) => INSTR_LD_RM(r1.num, m2.num)
      case ("LDZ", r1: OpRegister, m2: OpMemory) => INSTR_LDZ_RM(r1.num, m2.num)
      case ("LDNZ", r1: OpRegister, m2: OpMemory) => INSTR_LDNZ_RM(r1.num, m2.num)

  private def decodeJMP(mnemonic: String, op: Operand): Instruction =
    mnemonic match
      case "JMP" => decodeLD("LD",op,OpRegister(0))
      case "JMPZ" => decodeLD("LDZ",op,OpRegister(0))
      case "JMPNZ" => decodeLD("LDNZ",op,OpRegister(0))

  private def decodeOUT(mnemonic: String, op1: Operand, op2: Operand): Instruction =
    (mnemonic, op1, op2) match
      case ("OUT", r: OpRegister, p: OpPort) => INSTR_LD_RO(r.num, p.num)
      case ("OUTZ", r: OpRegister, p: OpPort) => INSTR_LDZ_RO(r.num, p.num)
      case ("OUTNZ", r: OpRegister, p: OpPort) => INSTR_LDNZ_RO(r.num, p.num)

  private def decodeIN(mnemonic: String, op1: Operand, op2: Operand): Instruction =
    (mnemonic, op1, op2) match
      case ("IN", r: OpRegister, p: OpPort) => INSTR_LD_RI(r.num, p.num)
      case ("INZ", r: OpRegister, p: OpPort) => INSTR_LDZ_RI(r.num, p.num)
      case ("INNZ", r: OpRegister, p: OpPort) => INSTR_LDNZ_RI(r.num, p.num)

  private def decodeALU2(mnemonic: String, op1: Operand, op2: Operand): Instruction =
    (mnemonic, op1, op2) match
      case ("ADD", r1: OpRegister, r2: OpRegister) => INSTR_ADD(r1.num, r2.num)
      case ("SUB", r1: OpRegister, r2: OpRegister) => INSTR_SUB(r1.num, r2.num)
      case ("CMP", r1: OpRegister, r2: OpRegister) => INSTR_CMP(r1.num, r2.num)
      case ("AND", r1: OpRegister, r2: OpRegister) => INSTR_AND(r1.num, r2.num)
      case ("OR", r1: OpRegister, r2: OpRegister) => INSTR_OR(r1.num, r2.num)
      case ("XOR", r1: OpRegister, r2: OpRegister) => INSTR_XOR(r1.num, r2.num)

  private def decodeALU1(mnemonic: String, op1: Operand): Instruction =
    (mnemonic, op1) match
      case ("INC", r: OpRegister) => INSTR_INC(r.num)
      case ("DEC", r: OpRegister) => INSTR_DEC(r.num)
  
  private def LD: Parser[Instruction] =
    ("LDNZ" | "LDZ" | "LD") ~ operand ~ operand ^^ { case m ~ op1 ~ op2 => decodeLD(m,op1,op2) }

  private def LDA: Parser[Instruction] =
    ("LDALNZ" | "LDAHNZ" | "LDALZ" | "LDAHZ" | "LDAL" | "LDAH") ~ operand ^^ { case m ~ op => decodeLDA(m, op) }

  private def JMP: Parser[Instruction] =
    ("JMPNZ" | "JMPZ" | "JMP") ~ operand ^^ { case m ~ o => decodeJMP(m,o) }

  private def OUT: Parser[Instruction] =
    ("OUTNZ" | "OUTZ" | "OUT") ~ operand ~ operand ^^ { case m ~ op1 ~ op2 => decodeOUT(m, op1, op2) }

  private def IN: Parser[Instruction] =
    ("INNZ" | "INZ" | "IN") ~ operand ~ operand ^^ { case m ~ op1 ~ op2 => decodeIN(m, op1, op2) }

  private def ALU2: Parser[Instruction] =
    ("ADD" | "SUB" | "CMP" | "AND" | "OR" | "XOR") ~ operand ~ operand ^^ { case m ~ op1 ~ op2 => decodeALU2(m, op1, op2) }

  private def ALU1: Parser[Instruction] =
    ("INC" | "DEC") ~ operand ^^ { case m ~ op => decodeALU1(m, op) }

  def instruction: Parser[Instruction] = LDA | LD | JMP | OUT | IN | ALU2 | ALU1

class LineParser extends BaseParser[Instruction] with InstructionParser:
  override def result: Parser[Instruction] = instruction
    
