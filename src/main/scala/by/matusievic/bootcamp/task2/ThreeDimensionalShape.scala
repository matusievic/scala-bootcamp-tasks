package by.matusievic.bootcamp.task2

sealed trait ThreeDimensionalShape extends Shape with ThreeDimensionalLocated with ThreeDimensionalBounded {
  override def dimensions: Int = 3

  def surfaceArea: Option[Double]

  def volume: Option[Double]
}

sealed trait ThreeDimensionalLocated {
  def x: Double

  def y: Double

  def z: Double
}

sealed trait ThreeDimensionalBounded {
  def minX: Double

  def maxX: Double

  def minY: Double

  def maxY: Double

  def minZ: Double

  def maxZ: Double
}

final case class ThreeDimensionalPoint(x: Double, y: Double, z: Double) extends ThreeDimensionalShape {
  override def minX: Double = x

  override def maxX: Double = x

  override def minY: Double = y

  override def maxY: Double = y

  override def minZ: Double = z

  override def maxZ: Double = z

  override def surfaceArea: Option[Double] = None

  override def volume: Option[Double] = None

}

final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends ThreeDimensionalShape {
  override def x: Double = centerX

  override def y: Double = centerY

  override def z: Double = centerZ

  override def minX: Double = centerX - radius

  override def maxX: Double = centerX + radius

  override def minY: Double = centerY - radius

  override def maxY: Double = centerY + radius

  override def minZ: Double = centerZ - radius

  override def maxZ: Double = centerZ + radius

  override def surfaceArea: Option[Double] = Some(4 * math.Pi * math.pow(radius, 2))

  override def volume: Option[Double] = Some((4 / 3) * math.Pi * math.pow(radius, 3))

}

final case class Cube(centerX: Double, centerY: Double, centerZ: Double, sideLength: Double) extends ThreeDimensionalShape {

  override def x: Double = centerX

  override def y: Double = centerY

  override def z: Double = centerZ

  override def minX: Double = centerX - (sideLength / 2)

  override def maxX: Double = centerX + (sideLength / 2)

  override def minY: Double = centerY - (sideLength / 2)

  override def maxY: Double = centerY + (sideLength / 2)

  override def surfaceArea: Option[Double] = Some(6 * math.pow(sideLength, 2))

  override def volume: Option[Double] = Some(math.pow(sideLength, 3))

  override def minZ: Double = centerZ - (sideLength / 2)

  override def maxZ: Double = centerZ + (sideLength / 2)

}
