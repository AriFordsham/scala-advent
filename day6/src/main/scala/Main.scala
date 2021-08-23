import scala.io.Source

import cats._
import cats.implicits._

@main def hello: Unit = 
  def fromFile(name : String) =
    Source.fromFile(name)
      .getLines
        .toVector

  val input = fromFile("input.txt")

  def groups[A](ls : Vector[String], op : (A, String) => A, z : A) =
    val folded = Foldable[Vector].foldLeft
      (ls, (Vector[A](), z))
      ((acc, line) =>
        acc match
          case (big, small) =>
            if line == ""
              then (big :+ small, z)
              else (big, op(small, line)) 
      )

    folded._1 :+ folded._2

    folded._1 :+ folded._2

  println("Part One: " + groups[String](input, _+_, "").map(_.toSet.size).sum)

  println("Part Two: " + groups[Vector[String]](input, _:+_, Vector[String]()).map(_.map(_.toSet).reduce(_&_).size).sum)