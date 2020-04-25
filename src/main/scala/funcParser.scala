import Parser.sParser

import scala.util.matching.Regex

class funcParser extends Parsers[Parser.sParser]{
  override def run[A](p: Parser.sParser[A])(input: String): Either[ParseError, A] = {
      p(Location(input, pos = 0)) match {
      case Success(get, _)=>Right(get)
      case Failure(e) => Left(e)
    }
  }

  override def flatMap[A, B](p: Parser.sParser[A])(f: A => Parser.sParser[B]): Parser.sParser[B] =
    input=> p(input) match {
      case Success(get,m)=>{
        input.pos = m
        f(get)(input)
      }
      case err@Failure(_)=>err
    }
  override def slice[A](p: Parser.sParser[A]): Parser.sParser[String] =
    x=> p(x) match {
      case Success(Location(input, pos,_), c)=>
        Success(input.slice(0, pos + 1), c)
      case fa@Failure(_)=>fa
    }


  override def or[A](s1: Parser.sParser[A], s2: Parser.sParser[A]): Parser.sParser[A] =
    loc=>s1(loc) match {
      case ans@Success(_,_)=>ans
      case Failure(_)=>s2(loc)
    }

  override implicit def string(s: String): Parser.sParser[String] =  {
    @scala.annotation.tailrec
    def parse(loc:Location):Result[String] = loc match {
      case loc@Location(input, pos, offset)=>
        if(offset < s.length && offset + pos < input.length
          && s.charAt(offset) == input.charAt(pos + offset))
            parse(Location(input, pos, offset + 1))
          else if(offset == s.length)
            Success(input.slice(pos, offset + pos), pos + offset)
          else
            Failure(loc.toError("Expect:"  + s + "but got:\n"
              + input.slice(pos + offset, input.length)))
    }
    loc=>parse(loc)
  }

  override implicit def regex(r: Regex): Parser.sParser[String] =
  {
    case loc@Location(input,pos,_)=>r.findFirstMatchIn(input.slice(pos - 1, input.length)) match {
      case Some(es@Regex.Match(str))=>
        if(es.start == 1)
          Success(str, es.end + pos - 1)
        else
          Failure(loc.toError("Prefix is not match:" + es.start))
      case None=>Failure(loc.toError("Require" + r.toString()))
    }
  }


  override def succeed[A](a: A): sParser[A] =
  {
    case Location(_,pos,_) =>Success(a, pos)
  }

  override def compose[A, B, C](f: A => sParser[B], g: B => sParser[C]): A => sParser[C] = ???
}
