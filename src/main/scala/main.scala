
import scala.util.matching.Regex


object main {
  def main(args:Array[String]):Unit = {
    val eParse = new funcParser
    import eParse._

    val json = "{\"name\":125}"

    val jparse = JSON.jsonParser(eParse)
    println(json)
    println(run(jparse)(json))
  }
}
