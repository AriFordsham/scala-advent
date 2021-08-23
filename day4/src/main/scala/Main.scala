import scala.io.Source
import scala.collection.immutable._

import cats._
import cats.implicits._
import cats.data._
import cats.parse._
import cats.compat.SortedSet

@main def hello: Unit = 
  def fromFile(name : String) =
    Source.fromFile(name)
      .getLines
        .toVector

  val input = fromFile("input.txt")

  def passes(l : Vector[String]) =
    val folded = Foldable[Vector].foldLeft
      (l, (Vector[String](), ""))
      ((acc, line) =>
        acc match
          case (big, small) =>
            if line == ""
              then (big :+ small, "")
              else (big, small + line + " ") 
      )

    folded._1 :+ folded._2

  val simpleParser =
    ((Parser.charsWhile(_.isLetter) <* Parser.char(':')) ~ Parser.charsWhile(_!=' ') <* Parser.char(' ')).rep

  val keys = SortedSet("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  println("Part One: " + passes(input)
    .map(
      simpleParser.parseAll(_)
        match {
          case Right(l) => l.map(_._1).toNes.toSortedSet
        }
    ).count(keys.subsetOf))

  def keyValue(key : String, value : Parser[Any]) =
    (Parser.string(key).as(key) <* Parser.char(':') <* value <* Parser.char(' ').rep).backtrack

  def years(id : String, min : Int, max : Int) =
    keyValue(id,
      Numbers
        .bigInt
          .filter(n => n >= min && n <= max)
    )
  
  val hgt = keyValue(
    "hgt",
    Parser.eitherOr(
      (Numbers.bigInt.filter(n => n >= 150 && n <= 193) ~ Parser.string("cm")).backtrack,
      (Numbers.bigInt.filter(n => n >= 59 && n <= 76) ~ Parser.string("in")).backtrack
    )
  )

  val hcl = keyValue(
    "hcl",
    Parser.char('#') ~ Parser.oneOf("0123456789abcdef".map(Parser.char).toList).rep(6,6)
  )

  val ecl =
    keyValue(
      "ecl",
      Parser.oneOf(List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").map(Parser.string(_).backtrack)))

  val pid = keyValue("pid", Numbers.digit.rep(9,9))

  val cid = keyValue("cid", Parser.charsWhile(_!=' '))

  val validParser = (Parser.oneOf(
    years("byr", 1920, 2002) ::
    years("iyr", 2010, 2020) ::
    years("eyr", 2020, 2030) ::
    hgt ::
    hcl ::
    ecl ::
    pid ::
    cid ::
    Nil
    )).rep

  print("Part Two: ")
  
  val valids = fromFile("valids.txt")
  val invalids = fromFile("invalids.txt")


  println(passes(input)
    .map(s =>
      validParser.parseAll(s)
        match {
          case (Right(l)) => Some(l.toNes.toSortedSet)
          case (Left(_))  => None
        }
    ).flatten.count(keys.subsetOf))