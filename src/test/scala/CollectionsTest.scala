import org.junit.jupiter.api.Test
import ex3.PerformanceUtils.*

import scala.collection.immutable
import scala.collection.mutable

class CollectionsTest:
  def toIterator[T <: Iterable[_]](constructor: () => T)(append: (T, Int) => Unit): T =
    val iterable = constructor()
    range.foreach(append(iterable, _))
    iterable

  def toSequenceImmutable[T <: immutable.Seq[_]](constructor: () => T)(range: Range.Inclusive): Unit =
      toIterator(constructor)(_.appended(_))

  def toSequenceMutable[T <: mutable.Seq[_]](constructor: () => T)(range: Range.Inclusive): Unit =
    toIterator(constructor)(_.appended(_))


  def toMapImmutable[T <: immutable.Map[String, Int]](constructor: () => T)(range: Range.Inclusive): Unit =
    toIterator(constructor)((map, el) => map.updated(el.toString, el))

  def toMapMutable[T <: mutable.Map[String, Int]](constructor: () => T)(range: Range.Inclusive): Unit =
    toIterator(constructor)((map, el) => map(el.toString) = el)


  def toSetImmutable[T <: immutable.Set[Int]](constructor: () => T)(range: Range.Inclusive): Unit =
    toIterator(constructor)(_ incl _)

  def toSetMutable[T <: mutable.Set[Int]](constructor: () => T)(range: Range.Inclusive): Unit =
    toIterator(constructor)(_ add _)


  private val maxValue = 10000000
  private val minValue = 1
  private val value = 10000001
  val range = minValue to maxValue

  @Test def linerSequence(): Unit =
    val listImmutable = measures("List Immutable")(range.toList)(_.last)(_.appended(value))(_.dropWhile(_ < maxValue))
    val listMutable = measures("List Buffer Immutable")(range.toBuffer)(_.last)(_.appended(value))(_.remove(0, maxValue - 1))
    listImmutable.foreach(map => assert(map._2 > listMutable(map._1)))

  @Test def indexSequence(): Unit =
    val vector = measures("Vector Immutable")(range.toVector)(_.last)(_.appended(value))(_.dropWhile(_ < maxValue))
    val array = measures("Array Immutable")(range.toArray)(_.last)(_.appended(value))(_.dropWhile(_ < maxValue))
    val arrayBuffer = measures("Array Buffer")(range.toBuffer)(_.last)(_.appended(value))(_.remove(0, maxValue - 1))

  @Test def set(): Unit = {}
//    val vector = measures("Vector Immutable")(range.toSet)(_.last)(_.appended(value))(_.dropWhile(_ < maxValue))
//    val arrayBuffer = measures("Array Buffer")(range.toBuffer)(_.last)(_.appended(value))(_.remove(0, maxValue - 1))

