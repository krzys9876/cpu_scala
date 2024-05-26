package org.kr.cpu

import assembler.Assembler

import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.ListHasAsScala
import org.kr.args.{ArgsAsClass, Argument}


@main
def main(args: String*): Unit =
  val progArgs = new Args(args.toArray)

  val filePath = Path.of(progArgs.asmFile)

  val lines: Vector[String] = Try(Files.readAllLines(filePath)) match
    case Success(lines) => lines.asScala.toVector
    case Failure(e) => println(f"Error occured reading file: ${filePath}\n${e.getClass.getName} / ${e.getMessage}")
      scala.sys.exit(1)

  println(f"${lines.length} lines read from file ${filePath.getFileName} (${filePath.toAbsolutePath})")

  val program = lines.mkString("\n")
  val assembler = Assembler(program)

  if(!assembler.isValid)
    println(assembler.errorMessage)
    scala.sys.exit(1)

  val code = assembler.machineCode.getOrElse(Vector())
  println(code.mkString("\n"))

  // clear screen
  /*print(27.toChar)
  print("[")
  print("2")
  print("J")

  // go home
  print(27.toChar)
  print("[")
  print("H")
*/
  val cpu = CpuHandlerImmutable.create
  val withProgram = code.foldLeft(cpu)((cpuProgrammed,line)=>
      cpuProgrammed.writeMemory(line.address,line.value.getOrElse(0.toShort)))

  val after = withProgram.handleNext(progArgs.programSteps)
  println("\n"+after.register.toString)
  if(progArgs.printMemoryFrom()>=0)
    val rows = (progArgs.printMemoryTo() - progArgs.printMemoryFrom()) / progArgs.printMemoryRow()
    for(row <- 0 until rows)
      println()
      for(col <- 0 until progArgs.printMemoryRow())
        print(f"${after.memory(progArgs.printMemoryFrom() + row * progArgs.printMemoryRow() + col)}%04X ")


class Args(args: Array[String]) extends ArgsAsClass(args) {
  val asmFile:Argument[String] = Argument.required
  private val stepsM:Argument[Double] = Argument.optional(-1.0)
  private val steps:Argument[Long] = Argument.optional(-1)
  val printMemoryFrom:Argument[Int] = Argument.optional(-1)
  val printMemoryTo:Argument[Int] = Argument.optional(-1)
  val printMemoryRow:Argument[Int] = Argument.optional(-1)

  parse()

  lazy val programSteps:Long =
    if(steps() > 0) steps
    else
      if(stepsM() <= 0.0 || stepsM() > (Long.MaxValue/1000000)) Long.MaxValue else (stepsM()*1000000.0).toLong
}

//TODO:
// test for non-existing label (eg. LDA some_random_label)