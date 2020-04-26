# sParser
Parser Combinator implemented in Scala

### The AST for JSON in Scala is represented as

```
case object JNull extends JSON
case class JNumber(get:Double) extends JSON
case class JString(get:String) extends JSON
case class JBool(get: Boolean) extends JSON
case class JArray(get:IndexedSeq[JSON]) extends JSON
case class JObject(get: Map[String, JSON]) extends JSON
```

### Parsing some JSON file as examples 

**This is an example json data**
```json
{"widget": {
    "debug": "on",
    "window": {
        "title": "Sample Konfabulator Widget",
        "name": "main_window",
        "width": 500,
        "height": 500
    },
    "image": { 
        "src": "Images/Sun.png",
        "name": "sun1",
        "hOffset": 250,
        "vOffset": 250,
        "alignment": "center"
    },
    "text": {
        "data": "Click Here",
        "size": 36,
        "style": "bold",
        "name": "text1",
        "hOffset": 250,
        "vOffset": 100,
        "alignment": "center",
        "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
    }
}}    
```
**We name it as "example" and run the parser**
 ```
println(run(jparse)(example))
 ```
 **We get**
 ```
 Right(JObject(Map("widget" -> JObject(Map("debug" -> JString("on"), "window" -> JObject(Map("title" -> JString("Sample Konfabulator Widget"), "name" -> JString("main_window"), "width" -> JNumber(500.0), "height" -> JNumber(500.0))), "image" -> JObject(HashMap("vOffset" -> JNumber(250.0), "alignment" -> JString("center"), "src" -> JString("Images/Sun.png"), "name" -> JString("sun1"), "hOffset" -> JNumber(250.0))), "text" -> JObject(HashMap("vOffset" -> JNumber(100.0), "data" -> JString("Click Here"), "size" -> JNumber(36.0), "alignment" -> JString("center"), "onMouseUp" -> JString("sun1.opacity = (sun1.opacity / 100) * 90;"), "style" -> JString("bold"), "name" -> JString("text1"), "hOffset" -> JNumber(250.0))))))))

 ```
 **The main class for the JSON parser is in the file JSON.scala**
