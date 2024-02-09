package org.kr.cpu

package assembler

import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

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
  def replace(map: Map[Operand, Operand]): Operand = map.getOrElse(this,this)

sealed trait Line:
  lazy val operands: Vector[Operand] = Vector()
  def replaceSymbols(map: Map[Operand, Operand]): Line = this
  def hasSymbols(map: Map[Operand, Operand]): Boolean = false
  lazy val size: Short = 0

case class EmptyLine() extends Line
case class LabelLine(label: Label) extends Line

case class DataLine(value: Vector[Operand]) extends Line:
  override lazy val operands: Vector[Operand] = value
  override def replaceSymbols(map: Map[Operand, Operand]): Line =
    copy(value = value.map(_.replace(map)))
  override def hasSymbols(map: Map[Operand, Operand]): Boolean = value.exists(v => map.keys.exists(_ == v))

case class SymbolLine(symbol: Operand, value: Operand) extends Line:
  override def replaceSymbols(map: Map[Operand, Operand]): Line = copy(value = value.replace(map))
  override def hasSymbols(map: Map[Operand, Operand]): Boolean = map.keys.exists(_ == value)

case class OrgLine(address: Operand) extends Line:
  override lazy val operands: Vector[Operand] = Vector(address)
  override def replaceSymbols(map: Map[Operand, Operand]): Line = copy(address = address.replace(map))
  override def hasSymbols(map: Map[Operand, Operand]): Boolean = map.keys.exists(_ == address)

case class Instruction0Line(mnemonic: Mnemonic0) extends Line:
  override lazy val size: Short = mnemonic.name match
    case "RET" => 3
    case _ => 1

case class Instruction1Line(mnemonic: Mnemonic1, oper: Operand) extends Line:
  override lazy val operands: Vector[Operand] = Vector(oper)
  override def replaceSymbols(map: Map[Operand, Operand]): Line = copy(oper = oper.replace(map))
  override def hasSymbols(map: Map[Operand, Operand]): Boolean = map.keys.exists(_ == oper)
  override lazy val size: Short = mnemonic.name match
    case "LDAZ" | "LDANZ" | "LDA" => 2
    case "CALL" => 7
    case "JMPIZ" | "JMPINZ" | "JMPI" => 3
    case _ => 1

case class Instruction2Line(mnemonic: Mnemonic2, oper1: Operand, oper2: Operand) extends Line:
  override lazy val operands: Vector[Operand] = Vector(oper1, oper2)
  override def replaceSymbols(map: Map[Operand, Operand]): Line =
    copy(oper1 = oper1.replace(map), oper2 = oper2.replace(map))
  override def hasSymbols(map: Map[Operand, Operand]): Boolean =
    map.keys.exists(k => k == oper1 || k == oper2)
  override lazy val size: Short = mnemonic.name match
    case "LDRZ" | "LDRNZ" | "LDR" => 3
    case _ => 1


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

  def labelOption(line: Line): Option[LabelLine] = line match
    case l: LabelLine => Some(l)
    case _ => None

  def orgOption(line: Line): Option[Operand] = line match
    case o: OrgLine => Some(o.address)
    case _ => None

  @tailrec
  def replaceSymbols(line: Line, map: Map[Operand, Operand], level: Int = 10): Either[String, Line] =
    level match
      case 0 => Left("too many nested symbols or circular reference")
      case _ =>
        if !line.hasSymbols(map) then Right(line)
        else replaceSymbols(line.replaceSymbols(map), map, level - 1)

class AssemblerParser extends BaseParser[Line] with LineParser:
  override def result: Parser[Line] = labelLine | dataLine | symbolLine | orgLine | instr0 | instr1 | instr2 | emptyLine

case class AddressedLine(address: Int, line: Line):
  private def toAtomicDefault: Vector[AtomicLine] = Vector(AtomicLine(address, line, this))

  def toAtomic: Vector[AtomicLine] =
    line match
      case SymbolLine(_, _) | OrgLine(_) | LabelLine(_) => toAtomicDefault
      case DataLine(values) => values.foldLeft((address,Vector[AtomicLine]()))((acc,v)=>
        (acc._1+1, acc._2 :+ AtomicLine(acc._1, DataLine(Vector(v)), this)))._2
      case Instruction0Line(_) | Instruction1Line(_, _) | Instruction2Line(_, _, _) => Assembler.expand(this)
      case _ => toAtomicDefault



case class AtomicLine(address: Int, line: Line, origLine: AddressedLine)

case class Assembler(input: String):
  private lazy val inputLines: Either[String,Vector[Line]] =
    val parsed = input.split("\n").toVector.map(AssemblerParser().process)
    Assembler.reduce(parsed)

  private lazy val nonEmptyLines = inputLines.map(_.filterNot(Line.isEmpty)).getOrElse(Vector())
  // symbol names mapped to values
  lazy val symbols: Map[Operand, Operand] = nonEmptyLines.flatMap(Line.symbolOption)
    .map(s => s.symbol -> s.value).toMap
  lazy val withSymbolsReplaced: Either[String, Vector[Line]] =
    val replaced = nonEmptyLines.map(Line.replaceSymbols(_,symbols))
    Assembler.reduce(replaced)

  lazy val withAddress: Either[String, Vector[AddressedLine]] =
    val calculatedAddressed = withSymbolsReplaced.getOrElse(Vector()).foldLeft(Right(0,Vector[AddressedLine]()).withLeft[String])((acc,line)=>
      acc match
        case Left(_) => acc
        case Right(accumulator) =>
          line match
            case OrgLine(addressText) => Assembler.getValue(addressText) match
              case Right(address) => Right((address+line.size, accumulator._2 :+ AddressedLine(address, line)))
              case Left(message) => Left(message)
            case _ => Right((accumulator._1+line.size, accumulator._2 :+ AddressedLine(accumulator._1, line))))
    calculatedAddressed match
      case Left(message) => Left(message)
      case Right((_, lines)) => Right(lines)

  // label names mapped to numeric addresses
  lazy val labels: Map[Operand, Operand] = withAddress.getOrElse(Vector()).map(l => (l.address,Line.labelOption(l.line)))
      // get only label lines
      .filter(_._2.isDefined)
      // remove colon and map name to address (formatted as hex)
      .map(l => Operand(l._2.get.label.name.replace(":","")) -> Operand(f"0x${l._1}%04X")).toMap

  lazy val withLabelsReplaced: Vector[AddressedLine] =
    // replacing labels is similar to replacing symbols (labels cannot be nested but it doesn't matter)
    withAddress.getOrElse(Vector()).map(l => l.copy(line = Line.replaceSymbols(l.line,labels).getOrElse(l.line)))

  /*lazy val withValuesValidated: Either[String, Vector[Line]] =
    withSymbolsReplaced.getOrElse(Vector()).foldLeft(Right(Vector[Line]()).withLeft[String])((acc, line) =>
      acc match
        case Left(_) => acc
        case Right(accumulator) =>
          line match
            case Instruction1Line(Mnemonic1(m), v) =>
              m match
                case "LDA" | "LDAZ" | "LDANZ" | "JMPI" | "JMPIZ" | "JMPINZ" | "CALL" =>
                  Assembler.getValue(v) match
                    case Left(message) => Left(message)
                    case Right(num) => Right(accumulator :+ line)
                case _ => Right(accumulator :+ line)
            case Instruction2Line(Mnemonic2(m), r, v) =>
              m match
                case "LDR" | "LDRZ" | "LDRNZ" =>
                  Assembler.getValue(v) match
                    case Left(message) => Left(message)
                    case Right(num) => Right(accumulator :+ line)
                case _ => Right(accumulator :+ line)
            case _ => Right(accumulator :+ line))
*/

  lazy val instructions: Vector[AtomicLine] =
    val toConvert = withLabelsReplaced
    toConvert.flatMap(_.toAtomic)

  lazy val isValid: Boolean = inputLines.isRight && withSymbolsReplaced.isRight

object Assembler:
  def reduce[T](in: Vector[Either[String,T]]): Either[String, Vector[T]] = in
    .foldLeft(Right(Vector[T]()).withLeft[String])((list, elem) => (list, elem) match
      case (Right(l), Right(e)) => Right(l :+ e)
      case (Right(_), Left(error)) => Left(error)
      case (Left(error), Right(_)) => Left(error)
      case (Left(error), Left(lnError)) => Left(f"$error\n$lnError"))

  private def getValue(address: Operand): Either[String,Int] =
    val (radix, value) = address.name.toLowerCase match
      case hex if hex.startsWith("0x") => (16, hex.substring(2))
      case bin if bin.startsWith("0b") => (2, bin.substring(2))
      case dec => (10, dec)
    Try(Integer.parseInt(value, radix).toShort) match
      case Success(addr) => Right(addr)
      case Failure(error) => Left(error.getMessage)

  def expandDefault(line: AddressedLine): Vector[AtomicLine] =  Vector(AtomicLine(line.address, line.line, line))

  def expandLDA(line: AddressedLine): Vector[AtomicLine] =
    line.line match
      case Instruction1Line(Mnemonic1(m), v) =>
        // TODO: probably this whole function should be converted to Either
        val value = Assembler.getValue(v).getOrElse(0)
        val valueL = (value & 0x00FF).toShort
        val valueH = ((value >> 8) & 0x00FF).toShort
        val (mnemonicL, mnemonicH) = m match
          case "LDA" => ("LDAL", "LDAH")
          case "LDAZ" => ("LDALZ", "LDAHZ")
          case "LDANZ" => ("LDALNZ", "LDAHNZ")
        Vector(
          AtomicLine(line.address, Instruction1Line(Mnemonic1(mnemonicL), Operand(f"0x$valueL%02X")), line),
          AtomicLine(line.address + 1, Instruction1Line(Mnemonic1(mnemonicH), Operand(f"0x$valueH%02X")), line))
      case _ => expandDefault(line)

  def expand(line: AddressedLine): Vector[AtomicLine] =
    line.line match
      case Instruction1Line(Mnemonic1(m),Operand(v)) if List("LDA","LDAZ","LDANZ").contains(m)  =>
        expandLDA(line)
      case _ => expandDefault(line)
        /*Vector(
          AtomicLine(line.address, Instruction1Line(Mnemonic1("LDAL"),Operand((v & 0xFF).toShort.toString)), line),
          AtomicLine(line.address+1, Instruction1Line(Mnemonic1("LDAH"),Operand(((v >> 8) & 0xFF).toShort.toString)), line))
      case Instruction1Line(Mnemonic1("LDAZ"), Operand(v)) =>
        Vector(
          AtomicLine(line.address, Instruction1Line(Mnemonic1("LDALZ"), Operand((v & 0xFF).toShort.toString)), line),
          AtomicLine(line.address + 1, Instruction1Line(Mnemonic1("LDAHZ"), Operand(((v >> 8) & 0xFF).toShort.toString)), line))
      case Instruction1Line(Mnemonic1("LDANZ"), Operand(v)) =>
        Vector(
          AtomicLine(line.address, Instruction1Line(Mnemonic1("LDALNZ"), Operand((v & 0xFF).toShort.toString)), line),
          AtomicLine(line.address + 1, Instruction1Line(Mnemonic1("LDAHNZ"), Operand(((v >> 8) & 0xFF).toShort.toString)), line))
      case Instruction2Line(Mnemonic2("LDR"), Operand(reg), Operand(value)) =>
        expand(AddressedLine(line.address, Instruction1Line(Mnemonic1("LDA"),Operand(value)))) :+
          AtomicLine(line.address + 2, Instruction2Line(Mnemonic2("LD"), Operand("R3"), Operand(reg)))*/
