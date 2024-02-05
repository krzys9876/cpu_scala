package org.kr.cpu

package assembler

import scala.util.parsing.combinator.JavaTokenParsers

abstract class Parser[T] extends JavaTokenParsers:
  def result: Parser[T]

  def process(input: String): Either[String, T] =
    parseAll(result, input) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)

trait TokenParser extends JavaTokenParsers:
  def dataKeyword: Parser[DataKeyword] = ".DATA" ^^ { _ => DataKeyword() }
  def symbolKeyword: Parser[SymbolKeyword] = ".SYMBOL" ^^ { _ => SymbolKeyword() }
  def orgKeyword: Parser[OrgKeyword] = ".ORG" ^^ { v => OrgKeyword() }
  private def mnemonic0name: Parser[String] = "NOP" | "RET"
  def mnemonic0: Parser[Mnemonic0] = mnemonic0name ^^ { n => Mnemonic0(n) }
  private def mnemonic1name: Parser[String] = "LDAZ" | "LDANZ" | "LDA" | "CALL" | "JMPZ" | "JMPNZ" | "JMP" | "INC" | "DEC"
  def mnemonic1: Parser[Mnemonic1] = mnemonic1name ^^ { n => Mnemonic1(n) }
  private def mnemonic2name: Parser[String] = "LDZ" | "LDNZ" | "LD" | "ADD" | "SUB" | "JMPNZ" | "JMP" | "INC" | "DEC"
  def mnemonic2: Parser[Mnemonic2] = mnemonic2name ^^ { n => Mnemonic2(n) }
  private def labelName: Parser[String] = """([a-zA-Z][a-zA-Z0-9_]*:)""".r
  def label: Parser[Label] = labelName ^^ {l => Label(l)}
  private def anyIdentifierOrValue: Parser[String] = """([a-zA-Z][a-zA-Z0-9_]*)""".r
  def operand: Parser[Operand] = anyIdentifierOrValue ^^ {t => Operand(t)}

  //def token: Parser[Token] = dataKeyword | symbolKeyword | orgKeyword |
  //  mnemonic0 | mnemonic1 | mnemonic2 | label | operand

trait LineParser extends TokenParser:
  private def labelLine: Parser[LabelLine] = label ^^ {l => LabelLine(l)}
  private def dataLine: Parser[DataLine] = dataKeyword ~> operand ^^ {v => DataLine(v)}
  private def symbolLine: Parser[SymbolLine] = symbolKeyword ~> operand ~ operand ^^ {case s ~ v => SymbolLine(s,v)}
  private def orgLine: Parser[OrgLine] = orgKeyword ~> operand ^^ {a => OrgLine(a)}
  private def instr0: Parser[Instruction0Line] = mnemonic0 ^^ {m => Instruction0Line(m)}
  private def instr1: Parser[Instruction1Line] = mnemonic1 ~ operand ^^ {case m ~ o => Instruction1Line(m, o)}
  private def instr2: Parser[Instruction2Line] = mnemonic2 ~ operand ~ operand ^^ {case m ~ o1 ~ o2 => Instruction2Line(m, o1, o2)}

  def line: Parser[Line] = labelLine | dataLine | symbolLine | orgLine | instr0 | instr1 | instr2

sealed trait Token

case class Label(name: String) extends Token
case class DataKeyword() extends Token
case class SymbolKeyword() extends Token
case class OrgKeyword() extends Token
case class Mnemonic0(name: String) extends Token
case class Mnemonic1(name: String) extends Token
case class Mnemonic2(name: String) extends Token
case class Operand(name: String) extends Token

sealed trait Line

case class LabelLine(label: Label) extends Line
case class DataLine(value: Operand) extends Line
case class SymbolLine(symbol: Operand, value: Operand) extends Line
case class OrgLine(address: Operand) extends Line
case class Instruction0Line(mnemonic: Mnemonic0) extends Line
case class Instruction1Line(mnemonic: Mnemonic1, oper: Operand) extends Line
case class Instruction2Line(mnemonic: Mnemonic2, oper1: Operand, oper2: Operand) extends Line

class AssemblerParser extends Parser[Line] with LineParser:
  override def result: Parser[Line] = line

