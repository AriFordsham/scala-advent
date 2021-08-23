import scala.io.Source


@main def hello: Unit = 
  def fromFile(name : String) =
    Source.fromFile(name)
      .getLines
        .toVector

  val input = fromFile("input.txt")

  val powers = 9 to 0 by -1

  def charToDigit(c : Char) =
    c match
      case 'B' => 1
      case 'F' => 0
      case 'L' => 0
      case 'R' => 1

  def seatNumber(s : String) =
    s.zip(powers).foldLeft(0)((acc, itm) => itm match {case (c, pwr) => (charToDigit(c) << pwr) | acc })

  println("Part Two: " + input.map(seatNumber).max)

  def findSeat(ss : Vector[String]) =
    val seats = ss.map(seatNumber).toSet

    (seats.min to seats.max).filter(!seats.contains(_)).max

  println("Part One: " + findSeat(input))