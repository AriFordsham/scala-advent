import scala.io.Source
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.Parsers

object PasswordParser extends JavaTokenParsers:
  def line =
    ((wholeNumber <~ "-" ^^ {_.toInt}) ~
      (wholeNumber ^^ {_.toInt}) ~ 
        ("[^:]".r <~ ":" ^^ {_.charAt(0)}) ~
        ".*".r) ^^ {case fst ~ snd ~ char ~ str => (fst, snd, char, str)} 

@main def hello: Unit = 
  import PasswordParser._

  val lines = Source.fromFile("input.txt").getLines.toList

  // val ans = lines.filter(
  //   l => parse(line, l) match
  //     case Success((min, max, char, str) , _) => {
  //       val n = str.count(_==char)
  //       n >= min && n <= max
  //     }
  // ).length

  def withParse(ls : List[String], f : (Int, Int, Char, String) => Boolean) =
    ls.filter(
      l => parse(line, l) match
      case Success(t , _) => f.tupled(t) 
    ).length

  println("Part One: " + withParse(lines, {(min, max, char, str) => {
    val n = str.count(_==char)
    n >= min && n <= max
  }}))

    println("Part Two: " + withParse(lines, {(fst, snd, char, str) => 
      str.charAt(fst-1) == char ^ str.charAt(snd-1) == char }))
