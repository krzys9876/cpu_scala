package org.kr.cpu

class OutputPort(val data:Vector[Short]):
  def put(value:Short)= new OutputPort(data :+ value)
  val size:Int=data.size
  def apply(pos:Int):Int=data(pos)

object OutputPort:
  val empty:OutputPort=new OutputPort(Vector())

class OutputFile(val files:Map[Short,OutputPort], val lastPort:Short=0, val lastValue:Short=0):
  def apply(port:Short,pos:Int):Int = 
    val file=getFile(port)
    if(file.size>pos) file(pos)
    else 0
    
  def write(port:Short, value:Short):OutputFile =
    print(value.toChar)
    val file=getFile(port).put(value)
    new OutputFile(files ++ Map(port->file),port,value)
  
  private def getFile(port:Short):OutputPort=files.getOrElse(port,OutputPort.empty)

object OutputFile:
  def blank:OutputFile= new OutputFile(Map())
