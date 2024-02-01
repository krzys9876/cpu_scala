package org.kr.cpu

trait OutputPort:
  def put(value:Short): OutputPort
  val size:Int
  def apply(pos:Int):Int
  def lastValue: Short


class OutputPortVector(val data:Vector[Short]) extends OutputPort:
  override def put(value:Short)= new OutputPortVector(data :+ value)
  override val size:Int=data.size
  override def apply(pos:Int):Int=data(pos)
  override lazy val lastValue: Short = data.headOption.getOrElse(0)

object OutputPortVector:
  val empty:OutputPort=new OutputPortVector(Vector())

class OutputFile(val files:Map[Short,OutputPort], val lastPort:Short=0, val lastValue:Short=0):
  def apply(port:Short,pos:Int):Int =
    val file=getFile(port)
    if(file.size>pos) file(pos)
    else 0

  def write(port:Short, value:Short):OutputFile =
    print(value.toChar)
    val file=getFile(port).put(value)
    new OutputFile(files ++ Map(port->file),port,value)

  private def getFile(port:Short):OutputPort=files.getOrElse(port,OutputPortVector.empty)

object OutputFile:
  def blank:OutputFile= new OutputFile(Map())

trait InputPort:
  def read: Short
  def refresh: InputPort

class InputPortVector(val data:Vector[Short], val default: Short = 0) extends InputPort:
  override def read: Short = data.headOption.getOrElse(default)
  override def refresh: InputPort = new InputPortVector(data.tail, default)
  
object InputPortVector:
  def single(value: Short): InputPortVector = new InputPortVector(Vector(value))

class InputFile(val map: Map[Short, InputPort] = Map(), default: Short = 0):
  def read(port: Short): (Short, InputFile) =
    map.get(port) match
      case None => (default, this)
      case Some(inputPort) => (inputPort.read, new InputFile(map + (port -> inputPort.refresh)))

  def attachPort(port: Short, inPort: InputPort): InputFile = new InputFile(map ++ Map(port -> inPort))

object InputFile:
  def blank:InputFile= new InputFile(Map())
