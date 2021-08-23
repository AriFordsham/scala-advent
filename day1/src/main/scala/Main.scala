import scala.io.Source

def cross[A](xs : List[List[A]], ys : List[List[A]]) =
  xs.map(x => ys.map(y => x ++ y)).flatten

def solve(xs : List[List[Int]]) =
  xs
    .filter(_.sum == 2020)
    .map(_.product)
    .toSet

@main def hello: Unit = 
  val lines = Source.fromFile("input.txt").getLines.toList

  val intLines = lines.map(_.toInt)

  val nestedLines = intLines.map(List(_))

  solve(cross(nestedLines, nestedLines))
    .foreach(i => println("Day 1: " + i))

  solve(cross(cross(nestedLines, nestedLines), nestedLines))
    .foreach(i => println("Day 2: " + i))

