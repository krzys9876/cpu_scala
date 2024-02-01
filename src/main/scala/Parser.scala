package org.kr.cpu

import scala.util.parsing.combinator.JavaTokenParsers

abstract class Parser[T] extends JavaTokenParsers:
  def result:Parser[T]

  def process(input: String): Either[String, T] =
    parseAll(result, input) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)


//trait CommonParser extends JavaTokenParsers
  //def integerNumber: Parser[String] = """(\d+)""".r
  //def hexNumber: Parser[String] = """(^0-9A-F$)""".r
  //def anyText: Parser[String] = """(.*)""".r
  //def anyTextQuoted: Parser[String] = stringLiteral ^^ { t => BaseParser.unEscapeBackslash(stripQuotes(t)) }
  //def emptyString: Parser[String] = """(^$)""".r

  //private def stripFirstAndLastChar(t: String): String = t.substring(1, t.length - 1)
  //private def stripQuotes(t: String): String =
  //  if (t.startsWith("\"") && t.endsWith("\"")) stripFirstAndLastChar(t) else t

trait Operand:
  val num: Short

case class OpRegister(override val num: Short) extends Operand
case class OpMemory(override val num: Short) extends Operand
case class OpPort(override val num: Short) extends Operand


trait InstructionParser extends Parser[Instruction]:
  private def regText: Parser[String] = "(R[0-9|A-F]{1})".r
  private def memText: Parser[String] = "(M[0-9|A-F]{1})".r
  private def portText: Parser[String] = "(P[0-9|A-F]{1})".r
  private def operandText: Parser[String] = "([R|M|P][0-9|A-F])".r
  def reg: Parser[OpRegister] = regText ^^ { r => OpRegister(Integer.parseInt(r.substring(1),0x10).toShort)}
  private def mem: Parser[OpMemory] = memText ^^ { m => OpMemory(Integer.parseInt(m.substring(1),0x10).toShort)}
  private def port: Parser[OpPort] = portText ^^ { p => OpPort(Integer.parseInt(p.substring(1),0x10).toShort)}
  private def operand: Parser[Operand] = operandText ^^ { op =>
    val num = Integer.parseInt(op.substring(1), 0x10).toShort
    op.charAt(0) match
      case 'R' => OpRegister(num)
      case 'M' => OpMemory(num)
      case 'P' => OpPort(num)}

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

  private def LD: Parser[Instruction] =
    ("LD" | "LDZ" | "LDNZ") ~ operand ~ operand ^^ { case m ~ op1 ~ op2 => decodeLD(m,op1,op2) }

  private def JMP: Parser[Instruction] =
    ("JMP" | "JMPZ" | "JMPNZ") ~ operand ^^ { case m ~ o => decodeJMP(m,o) }

  private def OUT: Parser[Instruction] =
    ("OUT" | "OUTZ" | "OUTNZ") ~ operand ~ operand ^^ { case m ~ op1 ~ op2 => decodeOUT(m, op1, op2) }

  private def IN: Parser[Instruction] =
    ("IN" | "INZ" | "INNZ") ~ operand ~ operand ^^ { case m ~ op1 ~ op2 => decodeIN(m, op1, op2) }

  def instruction: Parser[Instruction] = LD | JMP | OUT | IN

class LineParser extends Parser[Instruction] with InstructionParser:
  override def result: Parser[Instruction] = instruction
