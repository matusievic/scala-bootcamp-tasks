package by.matusievic.bootcamp.task7


object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)

  def main(args: Array[String]): Unit = {
    println(Seq(Money(1), Money(5), Money(3)).sorted)
  }
}

object Task2 {
  trait Show[T] {
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  implicit val ShowableUser: Show[User] = _.toString

  implicit class UserOps(val user: User) extends AnyVal {
    def show(implicit s: Show[User]): String = s.show(user)
  }

  def main(args: Array[String]): Unit = {
    println(User("1", "Oleg").show)
  }
}

// space-separated string parser
object Task3 {
  type Error = String
  trait Parse[T] {
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  implicit val parsableUser: Parse[User] = str => {
    str.split("\\s+").toList match {
      case id :: name :: Nil => Right(User(id, name))
      case _ => Left("Cannot parse")
    }
  }

  implicit class StringOps(val string: String) extends AnyVal {
    def parse[T](implicit p: Parse[T]): Either[Error, T] = p.parse(string)
  }

  def main(args: Array[String]): Unit = {
    println("1 John".parse[User])
    println("lalala".parse[User])
  }
}

// design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
object Task4 {
  trait Compare[A] {
    def ===(l: A, r: A): Boolean
  }

  implicit val ComparableInt: Compare[Int] = (l, r) => l == r
  implicit val ComparableString: Compare[String] = (l, r) => l == r

  implicit class IntOps(val i: Int) extends AnyVal {
    def ===(other: Int)(implicit c: Compare[Int]): Boolean = c.===(i, other)
  }

  implicit class StringOps(val s: String) extends AnyVal {
    def ===(other: String)(implicit c: Compare[String]): Boolean = c.===(s, other)
  }

  def main(args: Array[String]): Unit = {
    println(1 === 2)
    println(1 === 1)
    //    println(1 === "1")
    println("1" === "1")
    println("1" === "2")
    //    println("1" === 1)
    //    println(1.1 === 1.1)
  }
}

// create a typeclass for flatMap method
object AdvancedHomework {
  trait FlatMappable[Container[_]] {
    def fMap[A, B](c: Container[A], fun: A => Container[B]): Container[B]
  }

  implicit val flatMappableList: FlatMappable[List] = new FlatMappable[List] {
    override def fMap[A, B](c: List[A], fun: A => List[B]): List[B] = {
      c.foldLeft[List[B]](Nil)((acc, cur) => acc ++ fun(cur))
    }
  }

  implicit class ListOps[A](val l: List[A]) extends AnyVal {
    def fMap[B](fun: A => List[B])(implicit fm: FlatMappable[List]): List[B] = fm.fMap(l, fun)
  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3).fMap(i => List(i, i + 1)))
  }
}
