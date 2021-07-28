import scala.io.Source

@main def hello: Unit = 
  val lines = Source.fromFile("input.txt").getLines.toList


  def treesForSlope(rows : List[String], right : Int, down : Int) = {
    val width = rows match { case hd :: _ => hd.length }

    (1 to ((rows.length-1)/down))
      .map(i => rows(i*down).charAt((i*right) % width) == '#')
        .count(identity)
  }

  println("Part One: " + treesForSlope(lines, 3, 1))

  val part2 =
    List((1,1),(3,1),(5,1),(7,1),(1,2))
      .map({ case (r, d) => treesForSlope(lines, r, d).toLong })
        .product

  println("Part Two: " + part2)