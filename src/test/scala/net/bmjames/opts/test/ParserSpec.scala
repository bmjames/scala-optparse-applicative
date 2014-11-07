package net.bmjames.opts.test

import net.bmjames.opts._
import net.bmjames.opts.test.example.Commands
import net.bmjames.opts.types.{ParserResult, Failure, Success, ParserInfo}

import org.scalacheck.{Prop, Properties}
import scalaz.syntax.apply._

import example._

class ParserSpec extends Properties("Parser") {

  def run[A](pinfo: ParserInfo[A], args: List[String]): ParserResult[A] =
    execParserPure(prefs(), pinfo, args)

  property("dash-dash args") = Prop {
    val result = run(Commands.opts, List("hello", "foo", "--", "--bar", "--", "baz"))
    result match {
      case Success(Hello(List("foo", "--bar", "--", "baz"))) => true
      case _ => false
    }
  }

  val flags: Parser[Int] =
    flag_(1, long("foo")) <|> flag_(2, long("bar")) <|> flag_(3, long("baz"))

  property("disambiguate") = Prop {
    val result = execParserPure(prefs(disambiguate), info(flags), List("--f"))
    result == Success(1)
  }

  property("ambiguous") = Prop {
    val result = execParserPure(prefs(disambiguate), info(flags), List("--ba"))
    result match {
      case Success(_) => false
      case Failure(_) => true
    }
  }

  property("backtracking") = Prop {
    val p2 = switch(short('a'))
    val p1 = ^(subparser(command("c", info(p2))), switch(short('b')))((_, _))
    val i = info(p1 <*> helper)
    val result = execParserPure(prefs(noBacktrack), i, List("c", "-b"))
    result match {
      case Success(_) => false
      case Failure(_) => true
    }
  }

}
