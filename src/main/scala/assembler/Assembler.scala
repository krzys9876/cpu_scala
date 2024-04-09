package org.kr.cpu

package assembler

import parser.{AssemblerParser, LineParser}

import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}


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

trait Line:
  lazy val operands: Vector[Operand] = Vector()
  def replaceSymbols(map: Map[Operand, Operand]): Line = this
  def hasSymbols(map: Map[Operand, Operand]): Boolean = false
  lazy val size: Int = 0

case class EmptyLine() extends Line:
  override def toString: String = "[]"

case class LabelLine(label: Label) extends Line:
  override def toString: String = f"${label.name}"

case class DataLine(value: Vector[Operand]) extends Line:
  override lazy val operands: Vector[Operand] = value
  override def replaceSymbols(map: Map[Operand, Operand]): Line =
    copy(value = value.map(_.replace(map)))
  override def hasSymbols(map: Map[Operand, Operand]): Boolean = value.exists(v => map.keys.exists(_ == v))
  override def toString: String = f".DATA ${value.map(_.name).mkString(",")}"
  override lazy val size: Int = value.length

case class SymbolLine(symbol: Operand, value: Operand) extends Line:
  override def replaceSymbols(map: Map[Operand, Operand]): Line = copy(value = value.replace(map))
  override def hasSymbols(map: Map[Operand, Operand]): Boolean = map.keys.exists(_ == value)
  override def toString: String = f".SYMBOL ${symbol.name} = ${value.name}"

case class OrgLine(address: Operand) extends Line:
  override lazy val operands: Vector[Operand] = Vector(address)
  override def replaceSymbols(map: Map[Operand, Operand]): Line = copy(address = address.replace(map))
  override def hasSymbols(map: Map[Operand, Operand]): Boolean = map.keys.exists(_ == address)
  override def toString: String = f".ORG ${address.name}"

case class Instruction0Line(mnemonic: Mnemonic0) extends Line:
  override lazy val size: Int = mnemonic.name match
    case "RET" => 3
    case _ => 1
  override def toString: String = f"${mnemonic.name}"

case class Instruction1Line(mnemonic: Mnemonic1, oper: Operand) extends Line:
  override lazy val operands: Vector[Operand] = Vector(oper)
  override def replaceSymbols(map: Map[Operand, Operand]): Line = copy(oper = oper.replace(map))
  override def hasSymbols(map: Map[Operand, Operand]): Boolean = map.keys.exists(_ == oper)
  override lazy val size: Int = mnemonic.name match
    case "LDAZ" | "LDANZ" | "LDA" => 2
    case "CALL" => 7
    case "JMPIZ" | "JMPINZ" | "JMPI" => 3
    case _ => 1
  override def toString: String = f"${mnemonic.name} ${oper.name}"

case class Instruction2Line(mnemonic: Mnemonic2, oper1: Operand, oper2: Operand) extends Line:
  override lazy val operands: Vector[Operand] = Vector(oper1, oper2)
  override def replaceSymbols(map: Map[Operand, Operand]): Line =
    copy(oper1 = oper1.replace(map), oper2 = oper2.replace(map))
  override def hasSymbols(map: Map[Operand, Operand]): Boolean =
    map.keys.exists(k => k == oper1 || k == oper2)
  override lazy val size: Int = mnemonic.name match
    case "LDRZ" | "LDRNZ" | "LDR" => 3
    case _ => 1
  override def toString: String = f"${mnemonic.name} ${oper1.name} ${oper2.name}"


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

case class AddressedLine(address: Int, line: Line, origLine: InputLine):
  private def toAtomicDefault: Vector[AtomicLine] = Vector(AtomicLine(address, line, this))

  def toAtomic: Vector[AtomicLine] =
    line match
      case SymbolLine(_, _) | OrgLine(_) | LabelLine(_) => toAtomicDefault
      case DataLine(values) => values.foldLeft((address,Vector[AtomicLine]()))((acc,v)=>
        (acc._1+1, acc._2 :+ AtomicLine(acc._1, DataLine(Vector(v)), this)))._2
      case Instruction0Line(_) | Instruction1Line(_, _) | Instruction2Line(_, _, _) => Assembler.expand(this)
      case _ => toAtomicDefault
  override def toString: String = f"0x$address%04X ${line.toString.padTo(12,' ')} | ${origLine.toString}"

case class AtomicLine(address: Int, line: Line, origLine: AddressedLine):
  override def toString: String =
    val lineStr = line match
      case OrgLine(_) | LabelLine(_) | SymbolLine(_,_) => f"[${line.toString}]"
      case _=> f" ${line.toString}"
    f"0x$address%04X ${lineStr.padTo(13,' ')} | ${origLine.toString}"
    
  def parseInstruction(toParse: String): Either[String, MachineCodeLine] =
    LineParser().process(toParse) match
      case Left(message) => Left(message)
      case Right(instr) => Right(MachineCodeLine(address, Some(instr.value), Some(instr), this))
  
  def parseData(data: Vector[Operand]): Either[String, MachineCodeLine] =
    data match
      case d if d.size==1 => Assembler.getValue(d.head) match
        case Left(message) => Left(message)
        case Right(v) => Right(MachineCodeLine(address, Some(v.toShort), None, this))
      case _ => Left("Improper DATA line (should contain 1 value)")  
    
  def toMachineCode: Either[String, MachineCodeLine] =
    line match
      case Instruction0Line(m) => parseInstruction(f"${m.name}")
      case Instruction1Line(m,op) => parseInstruction(f"${m.name} ${op.name}")
      case Instruction2Line(m,op1,op2) => parseInstruction(f"${m.name} ${op1.name} ${op2.name}")
      case DataLine(value) => parseData(value)
      case _ => Right(MachineCodeLine(address, None, None, this))


case class MachineCodeLine(address: Int, value: Option[Short], instruction: Option[Instruction], origLine: AtomicLine):
  override def toString: String =
    val valueStr = if(value.isDefined) f"${value.get}%04X" else "[]"
    val instrStr = if(instruction.isDefined) instruction.get.toString else "[]"
    f"0x$address%04X ${valueStr.padTo(4,' ')} ${instrStr.padTo(10,' ')} | ${origLine.toString}"

case class InputLine(num: Int, line: String):
  override def toString: String = f"$num%04d $line"

case class ParsedLine(line: Line, origLine: InputLine):
  override def toString: String = f"${line.toString} | ${origLine.toString}"

case class Assembler(input: String):
  // Make line numbers 1-based
  lazy val inputLines: Vector[InputLine] = input.strip().split("\n").toVector.zipWithIndex.map(l => InputLine(l._2 + 1, l._1))

  lazy val parsedLines: Either[String,Vector[ParsedLine]] =
    val parsed = inputLines.map(l =>
      AssemblerParser().process(l.line) match
        case Left(message) => Left(message)
        case Right(pLine) => Right(ParsedLine(pLine, l)))
    Assembler.reduce(parsed)

  private lazy val nonEmptyLines = parsedLines.map(lines => lines.filterNot(line => Line.isEmpty(line.line))).getOrElse(Vector())
  // symbol names mapped to values
  lazy val symbols: Map[Operand, Operand] = nonEmptyLines.flatMap(l => Line.symbolOption(l.line))
    .map(s => s.symbol -> s.value).toMap
  lazy val withSymbolsReplaced: Either[String, Vector[ParsedLine]] =
    val replaced = nonEmptyLines.map(l =>
      Line.replaceSymbols(l.line,symbols) match
        case Left(message) => Left(message)
        case Right(rLine) => Right(ParsedLine(rLine,l.origLine)))
    Assembler.reduce(replaced)

  lazy val withAddress: Either[String, Vector[AddressedLine]] =
    val calculatedAddressed = withSymbolsReplaced.getOrElse(Vector()).foldLeft(Right(0,Vector[AddressedLine]()).withLeft[String])((acc,line)=>
      acc match
        case Left(_) => acc
        case Right(accumulator) =>
          line.line match
            case OrgLine(addressText) => Assembler.getValue(addressText) match
              case Right(address) => Right((address+line.line.size, accumulator._2 :+ AddressedLine(address, line.line, line.origLine)))
              case Left(message) => Left(message)
            case _ => Right((accumulator._1+line.line.size, accumulator._2 :+ AddressedLine(accumulator._1, line.line, line.origLine))))
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

  lazy val atomic: Vector[AtomicLine] =
    val toConvert = withLabelsReplaced
    toConvert.flatMap(_.toAtomic)

  lazy val machineCode: Either[String, Vector[MachineCodeLine]] =
    val mCode = atomic.map(_.toMachineCode)
    Assembler.reduce(mCode)
  
  lazy val isValid: Boolean = parsedLines.isRight && withSymbolsReplaced.isRight && machineCode.isRight

  lazy val errorMessage: String = 
    if(isValid) "" 
    else parsedLines match
      case Left(message) => f"Error occured in step 1: parsing:\n$message"
      case _ => withAddress match
        case Left(message) => f"Error occured in step 2: symbol decoding:\n$message"
        case _ => machineCode match
          case Left(message) => f"Error occured in step 3: machine code generation:\n$message"
          case _ => "Unexpected error"


object Assembler:
  def reduce[T](in: Vector[Either[String,T]]): Either[String, Vector[T]] = in
    .foldLeft(Right(Vector[T]()).withLeft[String])((list, elem) => (list, elem) match
      case (Right(l), Right(e)) => Right(l :+ e)
      case (Right(_), Left(error)) => Left(error)
      case (Left(error), Right(_)) => Left(error)
      case (Left(error), Left(lnError)) => Left(f"$error\n$lnError"))

  def getValue(address: Operand): Either[String,Int] =
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

  def expandLDR(line: AddressedLine): Vector[AtomicLine] =
    line.line match
      case Instruction2Line(Mnemonic2(m), reg, v) =>
        val (mnemonicA, mnemonicR) = m match
          case "LDR" => ("LDA", "LD")
          case "LDRZ" => ("LDAZ", "LDZ")
          case "LDRNZ" => ("LDANZ", "LDNZ")
        expandLDA(AddressedLine(line.address, Instruction1Line(Mnemonic1(mnemonicA), v), line.origLine)) :+
        AtomicLine(line.address+2, Instruction2Line(Mnemonic2(mnemonicR), Operand("R3"), reg), line)
      case _ => expandDefault(line)

  def expandJMPI(line: AddressedLine): Vector[AtomicLine] =
    line.line match
      case Instruction1Line(Mnemonic1(m), v) =>
        val (mnemonicA, mnemonicJ) = m match
          case "JMPI" => ("LDA", "JMP")
          case "JMPIZ" => ("LDAZ", "JMPZ")
          case "JMPINZ" => ("LDANZ", "JMPNZ")
        expandLDA(AddressedLine(line.address, Instruction1Line(Mnemonic1(mnemonicA), v), line.origLine)) :+
          AtomicLine(line.address + 2, Instruction1Line(Mnemonic1(mnemonicJ), Operand("R3")), line)
      case _ => expandDefault(line)

  def expandCALL(line: AddressedLine): Vector[AtomicLine] =
    line.line match
      case Instruction1Line(Mnemonic1("CALL"), v) =>
          expandLDA(AddressedLine(line.address, Instruction1Line(Mnemonic1("LDA"), Operand(f"0x${line.address+7}%04X")), line.origLine)) ++
          expandPUSH(AddressedLine(line.address + 2, Instruction1Line(Mnemonic1("PUSH"), Operand("R3")), line.origLine)) ++
          expandJMPI(AddressedLine(line.address + 4, Instruction1Line(Mnemonic1("JMPI"), v), line.origLine))
      case _ => expandDefault(line)

  def expandRET(line: AddressedLine): Vector[AtomicLine] =
    line.line match
      case Instruction0Line(Mnemonic0("RET")) =>
        expandPOP(AddressedLine(line.address, Instruction1Line(Mnemonic1("POP"), Operand("R3")), line.origLine)) :+
          AtomicLine(line.address + 2, Instruction1Line(Mnemonic1("JMP"), Operand("R3")), line)
      case _ => expandDefault(line)

  def expandPUSH(line: AddressedLine): Vector[AtomicLine] =
    line.line match
      case Instruction1Line(Mnemonic1("PUSH"), r) =>
        Vector(AtomicLine(line.address, Instruction1Line(Mnemonic1("DEC"), Operand("R1")), line),
          AtomicLine(line.address + 1, Instruction2Line(Mnemonic2("LD"), r, Operand("M1")), line))
      case _ => expandDefault(line)

  def expandPOP(line: AddressedLine): Vector[AtomicLine] =
    line.line match
      case Instruction1Line(Mnemonic1("POP"), r) =>
        Vector(AtomicLine(line.address, Instruction2Line(Mnemonic2("LD"), Operand("M1"), r), line),
          AtomicLine(line.address + 1, Instruction1Line(Mnemonic1("INC"), Operand("R1")), line))
      case _ => expandDefault(line)

  def expand(line: AddressedLine): Vector[AtomicLine] =
    line.line match
      case Instruction1Line(Mnemonic1(m),_) if List("LDA","LDAZ","LDANZ").contains(m) => expandLDA(line)
      case Instruction2Line(Mnemonic2(m),_,_) if List("LDR","LDRZ","LDRNZ").contains(m) => expandLDR(line)
      case Instruction1Line(Mnemonic1(m),_) if List("JMPI","JMPIZ","JMPINZ").contains(m) => expandJMPI(line)
      case Instruction1Line(Mnemonic1(m),_) if List("CALL").contains(m) => expandCALL(line)
      case Instruction1Line(Mnemonic1(m),_) if List("PUSH").contains(m) => expandPUSH(line)
      case Instruction1Line(Mnemonic1(m),_) if List("POP").contains(m) => expandPOP(line)
      case Instruction0Line(Mnemonic0(m)) if List("RET").contains(m) => expandRET(line)
      case _ => expandDefault(line)
