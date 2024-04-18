import org.junit.jupiter.api.Test
import ex3.PerformanceUtils.*
import ex3.PerformanceUtils.Exp
import org.junit.jupiter.api.Assertions.assertTrue

import scala.collection.immutable
import scala.collection.mutable
import scala.reflect.ClassTag

class CollectionsTest:
  private val maxValue = 10000000
  private val minValue = 1
  private val value = 10000001
  val range = minValue to maxValue

  @Test def linerSequence(): Unit =
    val immutableList = measures("List Immutable")(range.to(immutable.List))(_.last)(_ :+ value)(_.dropWhile(_ < maxValue))
    val mutableList = measures("List Buffer Immutable")(range.to(mutable.ListBuffer))(_.last)(_ += value)(_.dropWhile(_ < maxValue))

  @Test def indexSequence(): Unit =
    val vector = measures("Vector Immutable")(range.to(immutable.Vector))(_.last)(_ :+ value)(_.dropWhile(_ < maxValue))
    val array = measures("Array Immutable")(range.to(immutable.ArraySeq))(_.last)(_ :+ value)(_.dropWhile(_ < maxValue))
    val arrayBuffer = measures("Array Buffer")(range.to(mutable.ArrayBuffer))(_.last)(_ += value)(_.dropWhile(_ < maxValue))

  @Test def set(): Unit =
    val immutableSet = measures("Vector Immutable")(range.to(immutable.HashSet))(_.last)(_ + value)(_.dropWhile(_ < maxValue))
    val mutableSet = measures("Array Buffer")(range.to(mutable.HashSet))(_.last)(_ += value)(_.dropWhile(_ < maxValue))

  @Test def map(): Unit =
    val immutableMap = measures("Vector Immutable")(range.map(i => i -> i).to(immutable.HashMap))(_.last)(_.updated(value, value))(_.dropWhile((k, _) => k < maxValue))
    val mutableMap = measures("Array Buffer")(range.map(i => i -> i).to(mutable.HashMap))(_.last)(_ += (value -> value))(_.dropWhile((k, _) => k < maxValue))



