package org.kr.cpu

package assembler

import scala.util.parsing.combinator.JavaTokenParsers

abstract class BaseParser[T] extends JavaTokenParsers:
  def result: Parser[T]

  def process(input: String): Either[String, T] =
    parseAll(result, input) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)

object TokenParser:
  val mnemonic0list: List[String] = List("NOP", "RET", "JMPAZ", "JMPANZ", "JMPA")
  val mnemonic1list: List[String] = List("LDAZ", "LDANZ", "LDA", "CALL", "JMPIZ", "JMPINZ", "JMPI", "INC", "DEC")
  val mnemonic2list: List[String] = List("LDRZ", "LDRNZ", "LDR", "LDZ", "LDNZ", "LD", "ADD", "SUB", "AND", "OR", "CMP")

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

trait LineParser extends TokenParser:
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

sealed trait Token

case class Label(name: String) extends Token
case class DataKeyword() extends Token
case class SymbolKeyword() extends Token
case class OrgKeyword() extends Token
case class Mnemonic0(name: String) extends Token
case class Mnemonic1(name: String) extends Token
case class Mnemonic2(name: String) extends Token
case class Operand(name: String) extends Token:
  def replaceIfMatches(symbol: Operand, value: Operand): Operand = if(this.name==symbol.name) value else this

sealed trait Line:
  val operands: Vector[Operand] = Vector()
  
case class EmptyLine() extends Line
case class LabelLine(label: Label) extends Line
case class DataLine(value: Vector[Operand]) extends Line {override val operands: Vector[Operand] = value}
case class SymbolLine(symbol: Operand, value: Operand) extends Line
case class OrgLine(address: Operand) extends Line {override val operands: Vector[Operand] = Vector(address)}
case class Instruction0Line(mnemonic: Mnemonic0) extends Line
case class Instruction1Line(mnemonic: Mnemonic1, oper: Operand) 
  extends Line {override val operands: Vector[Operand] = Vector(oper)}
case class Instruction2Line(mnemonic: Mnemonic2, oper1: Operand, oper2: Operand) 
  extends Line {override val operands: Vector[Operand] = Vector(oper1, oper2)}

object Line:
  def isEmpty(line: Line): Boolean = line match
    case _: EmptyLine => true
    case _ => false

  def isSymbol(line: Line): Boolean = line match
    case _: SymbolLine => true
    case _ => false
    
  def symbolOption(line: Line): Option[SymbolLine] = line match
    case l: SymbolLine => Some(l)
    case _ => None

class AssemblerParser extends BaseParser[Line] with LineParser:
  override def result: Parser[Line] = labelLine | dataLine | symbolLine | orgLine | instr0 | instr1 | instr2 | emptyLine

case class Assembler(input: String):
  private lazy val inputLines = input.split("\n").toVector.map(AssemblerParser().process)
  lazy val isValid: Boolean = inputLines.forall(_.isRight)
  private lazy val nonEmptyLines = inputLines.map(_.getOrElse(EmptyLine())).filterNot(Line.isEmpty)
  lazy val symbols: Map[Operand, Operand] = nonEmptyLines.flatMap(Line.symbolOption)
    .map(s => s.symbol -> s.value).toMap
  
  