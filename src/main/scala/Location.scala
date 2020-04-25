
case class Location(input:String, var pos:Int, offset:Int = 0){
  lazy val line: Int = input.slice(0, pos + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, pos + 1).lastIndexOf('\n') match {
    case -1 => pos + 1
    case lineStart => pos - lineStart
  }
  def toError(msg:String):ParseError = ParseError(List((this, msg)))
}

