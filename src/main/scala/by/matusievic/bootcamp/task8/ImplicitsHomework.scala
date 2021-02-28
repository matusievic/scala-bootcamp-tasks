package by.matusievic.bootcamp.task8

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object ImplicitsHomework {
  /**
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)
      }
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {

      import syntax._

      private val map = mutable.LinkedHashMap.empty[K, V]

      private def cacheScoreSize: Int = map.keys.map(_.sizeScore).sum + map.values.map(_.sizeScore).sum

      @tailrec
      private def freeSpace(requiredSpace: Int, maxSize: Int): Unit = map.headOption match {
        case Some((key, _)) if cacheScoreSize + requiredSpace > maxSize =>
          map.remove(key)
          freeSpace(requiredSpace, maxSize)
        case _ => ()
      }

      def put(key: K, value: V): Unit = {
        val requiredSpace = key.sizeScore + value.sizeScore

        if (requiredSpace < maxSizeScore) {
          freeSpace(requiredSpace, maxSizeScore)
          map(key) = value
        }
      }

      def get(key: K): Option[V] = map.get(key)
    }

    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()

      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]

      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      implicit val mapIterate2: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keysIterator

        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.valuesIterator
      }
      implicit val packedMultiMapIterate2: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map { case (k, _) => k }.iterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map { case (_, v) => v }.iterator
      }

      implicit def byteGetSizeScore: GetSizeScore[Byte] = new GetSizeScore[Byte]() {
        override def apply(value: Byte): SizeScore = 1
      }

      implicit def intGetSizeScore: GetSizeScore[Int] = new GetSizeScore[Int]() {
        override def apply(value: Int): SizeScore = 4
      }

      implicit def longGetSizeScore: GetSizeScore[Long] = new GetSizeScore[Long]() {
        override def apply(value: Long): SizeScore = 8
      }

      implicit def charGetSizeScore: GetSizeScore[Char] = new GetSizeScore[Char]() {
        override def apply(value: Char): SizeScore = 2
      }

      implicit def stringGetSizeScore: GetSizeScore[String] = new GetSizeScore[String]() {
        override def apply(value: String): SizeScore = {
          val getSizeScore = implicitly[GetSizeScore[Char]].apply _
          12 + value.map(getSizeScore).sum
        }
      }

      implicit def iterateGetSizeScore[T: GetSizeScore, F[_] : Iterate]: GetSizeScore[F[T]] = new GetSizeScore[F[T]]() {
        override def apply(value: F[T]): SizeScore = {
          val getSizeScore: T => Int = implicitly[GetSizeScore[T]].apply
          12 + implicitly[Iterate[F]].iterator(value).map(getSizeScore).sum
        }
      }

      implicit def iterate2GetSizeScore[K: GetSizeScore, V: GetSizeScore, F[_, _] : Iterate2]: GetSizeScore[F[K, V]] = new GetSizeScore[F[K, V]] {
        override def apply(value: F[K, V]): SizeScore = {
          val iterate = implicitly[Iterate2[F]]
          val getSizeScoreKey: K => Int = implicitly[GetSizeScore[K]].apply
          val getSizeScoreValue: V => Int = implicitly[GetSizeScore[V]].apply
          12 + iterate.iterator1(value).map(getSizeScoreKey).sum + iterate.iterator2(value).map(getSizeScoreValue).sum
        }
      }
    }

  }

  object MyTwitter {

    import SuperVipCollections4s._
    import instances._
    import syntax._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )
    object Twit {
      implicit def twitGetSizeScore: GetSizeScore[Twit] = t => {
        t.id.sizeScore + t.userId.sizeScore + t.hashTags.sizeScore + t.attributes.sizeScore + t.fbiNotes.sizeScore
      }
    }

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )
    object FbiNote {
      implicit def fbiNoteGetSizeScore: GetSizeScore[FbiNote] = n => {
        n.month.sizeScore + n.favouriteChar.sizeScore + n.watchedPewDiePieTimes.sizeScore
      }
    }

    trait TwitCache {
      def put(twit: Twit): Unit

      def get(id: Long): Option[Twit]
    }

    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      private val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = cache.put(twit.id, twit)

      override def get(id: Long): Option[Twit] = cache.get(id)
    }
  }
}
