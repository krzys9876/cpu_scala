package org.kr.cpu
package parser

import assembler.*

import scala.util.parsing.combinator.JavaTokenParsers

object TokenParser:
  val mnemonic0list: List[String] = List("NOP", "RET", "JMPAZ", "JMPANZ", "JMPA")
  val mnemonic1list: List[String] = List("LDAZ", "LDANZ", "LDA", "CALL", "JMPIZ", "JMPINZ", "JMPI", "INC", "DEC", "PUSH", "POP")
  val mnemonic2list: List[String] = List("LDRZ", "LDRNZ", "LDR", "LDZ", "LDNZ", "LD", "ADD", "SUB", "AND", "OR", "CMP", "IN", "OUT")

trait TokenParser extends JavaTokenParsers:
  private def anyOf(elems:List[String]): Parser[String] =
    assume(elems.length > 1)
    val start: Parser[String] = elems.head
    elems.tail.foldLeft(start)((l,t)=> l | t)

  def dataKeyword: Parser[DataKeyword] = ".DATA" ^^ { _ => DataKeyword() }
  def symbolKeyword: Parser[SymbolKeyword] = ".SYMBOL" ^^ { _ => SymbolKeyword() }
  def orgKeyword: Parser[OrgKeyword] = ".ORG" ^^ { v => OrgKeyword() }
  private def mnemonic0name: Parser[String] = anyOf(TokenParser.mnemonic0list)
  def mnemonic0: Parser[Mnemonic0] = mnemonic0name ^^ { n => Mnemonic0(n) }
  private def mnemonic1name: Parser[String] = anyOf(TokenParser.mnemonic1list)
  def mnemonic1: Parser[Mnemonic1] = mnemonic1name ^^ { n => Mnemonic1(n) }
  private def mnemonic2name: Parser[String] = anyOf(TokenParser.mnemonic2list)
  def mnemonic2: Parser[Mnemonic2] = mnemonic2name ^^ { n => Mnemonic2(n) }
  private def labelName: Parser[String] = """([a-zA-Z][a-zA-Z0-9_]*:)""".r
  def label: Parser[Label] = labelName ^^ {l => Label(l)}
  private def anyIdentifierOrValue: Parser[String] = """([a-zA-Z0-9_:]+)""".r
  def operand: Parser[Operand] = anyIdentifierOrValue ^^ {t => Operand(t)}

trait AssemblerLineParser extends TokenParser:
  private def comment: Parser[String] = """(#.*)""".r
  private def sep: String = ","

  def emptyLine: Parser[EmptyLine] = opt(comment) ^^ {_ => EmptyLine()}
  def labelLine: Parser[LabelLine] = label <~ opt(comment) ^^ {l => LabelLine(l)}
  def dataLine: Parser[DataLine] = dataKeyword ~> rep1sep(operand,sep) <~ opt(comment) ^^ {v => DataLine(v.toVector)}
  def symbolLine: Parser[SymbolLine] = symbolKeyword ~> operand ~ "=" ~ operand <~ opt(comment) ^^ {case s ~ _ ~ v => SymbolLine(s,v)}
  def orgLine: Parser[OrgLine] = orgKeyword ~> operand <~ opt(comment) ^^ {a => OrgLine(a)}
  def instr0: Parser[Instruction0Line] = mnemonic0 <~ opt(comment) ^^ {m => Instruction0Line(m)}
  def instr1: Parser[Instruction1Line] = mnemonic1 ~ operand <~ opt(comment) ^^ {case m ~ o => Instruction1Line(m, o)}
  def instr2: Parser[Instruction2Line] = mnemonic2 ~ operand ~ sep ~ operand <~ opt(comment) ^^ {case m ~ o1 ~ _ ~ o2 => Instruction2Line(m, o1, o2)}

class AssemblerParser extends BaseParser[Line] with AssemblerLineParser:
  override def result: Parser[Line] = labelLine | dataLine | symbolLine | orgLine | instr0 | instr1 | instr2 | emptyLine
