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


trait InstructionParser extends Parser[Instruction]:
  private def regText: Parser[String] = """(R[0-9|A-F]{1})""".r
  private def memText: Parser[String] = """(M[0-9|A-F]{1})""".r
  private def portText: Parser[String] = """(P[0-9|A-F]{1})""".r
  def reg: Parser[Short] = regText ^^ { r => Integer.parseInt(r.substring(1),0x10).toShort}
  private def mem: Parser[Short] = memText ^^ { m => Integer.parseInt(m.substring(1),0x10).toShort}
  private def port: Parser[Short] = portText ^^ { p => Integer.parseInt(p.substring(1),0x10).toShort}

  private def LD: Parser[Instruction] =
    "LD" ~> reg ~ reg ^^ { case r1 ~ r2 => INSTR_LD_RR(r1,r2) } |
      "LD" ~> mem ~ reg ^^ { case m ~ r => INSTR_LD_MR(m, r) } |
      "LD" ~> reg ~ mem ^^ { case r ~ m => INSTR_LD_RM(r, m) }

  private def OUT: Parser[Instruction] =
    "OUT" ~> reg ~ port ^^ { case r ~ p => INSTR_LD_RO(r,p) }

  private def IN: Parser[Instruction] =
    "IN" ~> reg ~ port ^^ { case r ~ p => INSTR_LD_RI(r,p) }

  private def JMP: Parser[Instruction] =
    "JMP" ~> reg ^^ { r => INSTR_LD_RR(r,0) } |
      "JMP" ~> mem ^^ { m => INSTR_LD_MR(m, 0) }

  def instruction: Parser[Instruction] = LD | JMP | OUT | IN

class LineParser extends Parser[Instruction] with InstructionParser:
  override def result: Parser[Instruction] = instruction

class TParser extends Parser[(Short,Short)]:
  private def regText: Parser[String] = """(R[0-9|A-F])""".r
  def reg: Parser[Short] = regText ^^ { r => Integer.parseInt(r.substring(1),0x10).toShort}

  override def result: Parser[(Short,Short)] = reg ~ reg ^^ { case r1 ~ r2 => (r1, r2) }