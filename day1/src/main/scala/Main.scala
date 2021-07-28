import scala.io.Source

def product[A, B](xs : List[A], ys : List[B]) =
  xs.map(x => ys.map(y => (x, y))).flatten

@main def hello: Unit = 
  val lines = Source.fromFile("input.txt").getLines.toList

  val intLines = lines.map(_.toInt)

  // product(product(lines, lines), lines).filter((xy, z) => xy._1.toInt + xy._2.toInt + z.toInt == 2020) foreach println

  product(intLines, intLines)
    .filter((x, y) => x + y == 2020)
      .map((x, y) => x * y) 
        .toSet
          .foreach(i => println("Day 1: " + i))

  product(product(intLines, intLines), intLines)
    .filter((xy, z) => xy._1 + xy._2 + z == 2020)
      .map((xy, z) => xy._1 * xy._2 * z)
        .toSet
          .foreach(i => println("Day 2: " + i))

