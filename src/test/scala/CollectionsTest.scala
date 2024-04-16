import org.junit.jupiter.api.Test
import ex3.PerformanceUtils.*

import scala.collection.mutable.ArrayBuffer

class CollectionsTest:
  // Create
  // Read
  // Update
  // Delete

  private val maxValue = 10000000
  private val minValue = 1
  private val value = 6
  val range = minValue to maxValue

  @Test def linerSequence(): Unit =
    val listImmutable = measures("List Immutable")(range.toList)(_.last)(_.appended(value))(_.dropWhile(_ < maxValue))
    val listMutable = measures("List Buffer Immutable")(range.toBuffer)(_.last)(_.appended(value))(_.remove(0, maxValue - 1))
    listImmutable.foreach(map => assert(map._2 > listMutable(map._1)))

  @Test def indexSequence(): Unit =
    val vector = measures("Vector Immutable")(range.toVector)(_.last)(_.appended(value))(_.dropWhile(_ < maxValue))
    val array = measures("Array Immutable")(range.toArray)(_.last)(_.appended(value))(_.dropWhile(_ < maxValue))
    val arrayBuffer = measures("Array Buffer")(range.toBuffer)(_.last)(_.appended(value))(_.remove(0, maxValue - 1))


