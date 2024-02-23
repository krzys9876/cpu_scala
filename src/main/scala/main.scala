package org.kr.cpu

import assembler.Assembler

import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.ListHasAsScala

@main
def main(args: String*): Unit =
  if(args.length != 1)
    println(f"provide a file name with assembly code")
    scala.sys.exit(1)

  val filePath = Path.of(args(0))

  val lines: Vector[String] = Try(Files.readAllLines(filePath)) match
    case Success(lines) => lines.asScala.toVector
    case Failure(e) => println(f"Error occured reading file: ${filePath}\n${e.getClass.getName} / ${e.getMessage}")
      scala.sys.exit(1)

  println(f"${lines.length} lines read from file ${filePath.getFileName} (${filePath.toAbsolutePath})")

  val program = lines.mkString("\n")
  val assembler = Assembler(program)

  assembler.machineCode match
    case Right(lines) => println(lines.mkString("\n"))
    case Left(message) => println(f"Error occured while parsing the program:\n$message")
      scala.sys.exit(1)

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
  val withProgram = assembler.machineCode.getOrElse(Vector())
    .foldLeft(cpu)((cpuProgrammed,line)=>
      cpuProgrammed.writeMemory(line.address,line.value.getOrElse(0.toShort)))

  val after = withProgram.handleNext(100)
  println("\n"+after.register.toString)