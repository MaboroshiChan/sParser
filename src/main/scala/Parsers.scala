import scala.util.matching.Regex


trait Parsers[Parser[+_]] {self=>
  def run[A](p:Parser[A])(input:String):Either[ParseError,A]
  def char(c:Char):Parser[Char]  =
    map(string(c.toString))(_.charAt(0))



  def listOfN[A](n:Int, p:Parser[A]):Parser[List[A]] =
    if (n > 0) map2(p,listOfN(n - 1, p))((x, l)=>x::l)
    else succeed(List())

  def many[A](p:Parser[A]):Parser[List[A]] =
    map2(p, many(p))(_::_) | succeed(List())

  def many1[A](p:Parser[A]):Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A,B](a:Parser[A])(f:A=>B):Parser[B] =
    a.flatMap(x=>succeed(f(x)))
  def map2[A, B, C](p:Parser[A], p2: =>Parser[B])(f:(A,B)=>C):Parser[C] =
    p.flatMap(x=>p2.flatMap(y=>succeed(f(x, y))))

  def join[A](a:Parser[Parser[A]]):Parser[A] =
    a.flatMap(x=>x.flatMap(y=>succeed(y)))

  def product[A, B](p1: Parser[A], p2: =>Parser[B]):Parser[(A, B)] =
    flatMap(p1)(x=>flatMap(p2)(y=>succeed(x,y)))


  def flatMap[A, B](p:Parser[A])(f:A=>Parser[B]):Parser[B]
  def succeed[A](a:A):Parser[A]
  def slice[A](p:Parser[A]):Parser[String]

  def compose[A,B,C](f:A=>Parser[B], g:B=>Parser[C]):A=>Parser[C]

  def _flatMap[A,B](p:Parser[A])(f:A=>Parser[B]):Parser[B] =
    compose((_:Unit)=>p,f)()

  def or[A](s1:Parser[A], s2:Parser[A]):Parser[A]

  implicit def string(s:String):Parser[String]
  implicit def operators[A](p:Parser[A]):ParserOps[A] = ParserOps[A](p)
  implicit def asString[A](a:A)(implicit f:A=>Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]
  case class ParserOps[A](p:Parser[A]){
    def |[B>:A] (p2:Parser[B]):Parser[B] = self.or(p,p2)
    def or[B>:A](p2: =>Parser[B]):Parser[B] = self.or(p,p2)
    def **[B](p2: =>Parser[B]): Parser[(A,B)] =
      self.product(p,p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)
    def map[B](f:A=>B):Parser[B] =
      self.map(p)(f)
  }
}