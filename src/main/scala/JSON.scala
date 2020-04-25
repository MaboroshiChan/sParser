import scala.util.matching.Regex
trait JSON
object JSON{
  case object JNull extends JSON
  case class JNumber(get:Double) extends JSON
  case class JString(get:String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get:IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P:Parsers[Parser]):Parser[Any] = {
    import P._
    val spaces = many(" "|"\n")
    val comma = char(',') ** spaces
    val left = char('[') ** spaces
    val right = char(']') ** spaces
    val Left = char('{') ** spaces
    val Right = char('}') ** spaces
    val colon = char(':') ** spaces
    val dig = "0" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "1"




    val JNullP = string("null").map(_ =>JNull)
    val JNumP:Parser[JNumber] = many1(dig).map(_.mkString("")).map(x=>JNumber(x.toDouble))
    val JStr:Parser[JString] = regex("\".*\"".r).map(x=>JString(x))
    val JBP:Parser[JBool] =( string("true") | string("false") ).map(x=>JBool(x.toBoolean))
    def Tokens:Parser[JSON] =  JBP|JNumP|JArrayP|JStr|JObj|JNullP

    def Item = regex("\"[a-zA-z0-9][a-zA-z0-9]*\"".r) ** colon ** Tokens ** spaces

    def JObj:Parser[JObject] =  (Left ** Item ** many(comma ** Item) ** Right)
      .map{case (((_,h),t),_)=> h::t.map{case (_,item)=> item}}
      .map(arr=>arr.map{case (((str, _), ob), _)=> (str, ob)}.toMap)
      .map(x=>JObject(x))

    def JArrayP:Parser[JArray] = (left ** (JObj|Tokens) ** many(comma ** (JObj|Tokens)) ** right)
      .map{case (((_,h), t),_) => h::t.map{case(_,y)=>y}}
      .map(arr=>JArray(arr.toIndexedSeq))

    JObj
  }

  def main(args:Array[String]):Unit = {
    val eParse = new funcParser
    import eParse._

    val expar = many("a")
    val jparse = JSON.jsonParser(eParse)
    val input = ""
    val sjo = "\"Name\":1257"
    val json = "{\"Name\":\"Tachibana\"}"
    val json2 = "{\"IsOK\":false}"
    val json3 = "{\"Number\":12313}"
    val json4 = "{\"arr\":[\"one\",\"two\"], \"new\":{\"is\":false, \"is\":true, \"brother\":{\"little\":1, \"No\":false},\"Tokasaki\":\"Kurumi\"}}"
    val json5 = "{\"text\":{\"a1\":\"b2\"}}"
    val simplified = "{\n  \"name\":\"John\",\n  \"age\":30,\"car\": {\"car1\":\"Ford\",\"car2\":\"BMW\",\"car3\":\"Fiat\"}}"
    val example  = "{\n  \"name\":\"John\",\n  \"age\":null,\n  \"cars\": {\n    \"cart\":\"Ford\",\n    \"carr\":\"BMW\",\n    \"carrs\":\"Fiat\" \n}\n }"
    val example2 = "{\"menu\": {\n  \"id\": \"file\",\n  \"value\": \"File\",\n  \"popup\": {\n    \"menuitem\": [\n      {\"value\": \"New\", \"onclick\": \"CreateNewDoc\"},\n      {\"value\": \"Open\", \"onclick\": \"OpenDoc\"},\n      {\"value\": \"Close\", \"onclick\": \"CloseDoc\"}\n    ]\n  }\n}}"
    val example3 = "{\"widget\": {\n    \"debug\": \"on\",\n    \"window\": {\n        \"title\": \"Sample Konfabulator Widget\",\n        \"name\": \"main_window\",\n        \"width\": 500,\n        \"height\": 500\n    },\n    \"image\": { \n        \"src\": \"Images/Sun.png\",\n        \"name\": \"sun1\",\n        \"hOffset\": 250,\n        \"vOffset\": 250,\n        \"alignment\": \"center\"\n    },\n    \"text\": {\n        \"data\": \"Click Here\",\n        \"size\": 36,\n        \"style\": \"bold\",\n        \"name\": \"text1\",\n        \"hOffset\": 250,\n        \"vOffset\": 100,\n        \"alignment\": \"center\",\n        \"onMouseUp\": \"sun1.opacity = (sun1.opacity / 100) * 90;\"\n    }\n}}    "
    val example4 = "{\"menu\": {\n    \"header\": \"SVG Viewer\",\n    \"items\": [\n        {\"id\": \"Open\"},\n        {\"id\": \"OpenNew\", \"label\": \"Open New\"},\n        null,\n        {\"id\": \"ZoomIn\", \"label\": \"Zoom In\"},\n        {\"id\": \"ZoomOut\", \"label\": \"Zoom Out\"},\n        {\"id\": \"OriginalView\", \"label\": \"Original View\"},\n        null,\n        {\"id\": \"Quality\"},\n        {\"id\": \"Pause\"},\n        {\"id\": \"Mute\"},\n        null,\n        {\"id\": \"Find\", \"label\": \"Find...\"},\n        {\"id\": \"FindAgain\", \"label\": \"Find Again\"},\n        {\"id\": \"Copy\"},\n        {\"id\": \"CopyAgain\", \"label\": \"Copy Again\"},\n        {\"id\": \"CopySVG\", \"label\": \"Copy SVG\"},\n        {\"id\": \"ViewSVG\", \"label\": \"View SVG\"},\n        {\"id\": \"ViewSource\", \"label\": \"View Source\"},\n        {\"id\": \"SaveAs\", \"label\": \"Save As\"},\n        null,\n        {\"id\": \"Help\"},\n        {\"id\": \"About\", \"label\": \"About Adobe CVG Viewer...\"}\n    ]\n}}"
    println(run(jparse)(json))
    println(run(jparse)(json2))
    println(run(jparse)(json3))
    println(run(expar)(input))
    println(json4)
    println(run(jparse)(json5))
    println(run(jparse)(json4))
    println(run(jparse)(simplified))
    println(run(jparse)(example))
    println(run(jparse)(example2))
    println(run(jparse)(example3))
    println(run(jparse)(example4))
  }
}