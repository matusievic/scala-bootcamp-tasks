package by.matusievic.bootcamp.task7

object TypeclassTask {

  trait HashCode[T] {
    def hash(entry: T): Int
  }

  object HashCode {
    def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
  }

  implicit class HashCodeSyntax[A: HashCode](x: A) {
    def hash: Int = HashCode[A].hash(x)
  }

  implicit val hashCodableString: HashCode[String] = _.map(_.toInt).sum

  def main(args: Array[String]): Unit = {
    println("abc".hash)
  }
}
