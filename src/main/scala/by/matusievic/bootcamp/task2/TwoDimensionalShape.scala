package by.matusievic.bootcamp.task2

sealed trait TwoDimensionalShape extends Shape with TwoDimensionalLocated with TwoDimensionalBounded {
  override def dimensions: Int = 2

  def area: Option[Double]
}

sealed trait TwoDimensionalLocated {
  def x: Double

  def y: Double
}

sealed trait TwoDimensionalBounded {
  def minX: Double

  def maxX: Double

  def minY: Double

  def maxY: Double
}

final case class TwoDimensionalPoint(x: Double, y: Double) extends TwoDimensionalShape {
  override def minX: Double = x

  override def maxX: Double = x

  override def minY: Double = y

  override def maxY: Double = y

  override def area: Option[Double] = None
}

final case class Circle(centerX: Double, centerY: Double, radius: Double) extends TwoDimensionalShape {
  override def x: Double = centerX

  override def y: Double = centerY

  override def minX: Double = centerX - radius

  override def maxX: Double = centerX + radius

  override def minY: Double = centerY - radius

  override def maxY: Double = centerY + radius

  override def area: Option[Double] = Some(math.Pi * math.pow(radius, 2))
}

final case class Square(centerX: Double, centerY: Double, sideLength: Double) extends TwoDimensionalShape {
  override def area: Option[Double] = Some(math.pow(sideLength, 2))

  override def x: Double = centerX

  override def y: Double = centerY

  override def minX: Double = centerX - (sideLength / 2)

  override def maxX: Double = centerX + (sideLength / 2)

  override def minY: Double = centerY - (sideLength / 2)

  override def maxY: Double = centerY + (sideLength / 2)
}
