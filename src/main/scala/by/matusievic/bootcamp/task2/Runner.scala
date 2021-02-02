package by.matusievic.bootcamp.task2

object Runner {
  def main(args: Array[String]): Unit = {
    Seq(
      TwoDimensionalPoint(1, 2),
      Circle(0, 0, 1),
      Square(5, -1, 5)
    ).foreach(printShape)

    Seq(
      ThreeDimensionalPoint(1, 2, 3),
      Sphere(0, 0, 0, 1),
      Cube(5, -1, 3, 5)
    ).foreach(printShape)
  }

  def printShape(shape: TwoDimensionalShape): Unit = println(f"$shape%-50s, s = ${shape.area}%30s")
  def printShape(shape: ThreeDimensionalShape): Unit = println(f"$shape%-50s, s = ${shape.surfaceArea}%30s, v = ${shape.volume}%30s")
}
