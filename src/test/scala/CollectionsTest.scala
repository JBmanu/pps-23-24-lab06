import org.junit.jupiter.api.Test
import ex3.PerformanceUtils.*

class CollectionsTest:
  // Create
  // Read
  // Update
  // Delete

  private val maxValue = 10000000
  private val minValue = 1
  val list = (minValue to maxValue)

  @Test def linerSequence(): Unit =
    val listImmutable = measures("List Immutable")(list.toList)(_.last)(_.appended(6))(_.dropWhile(_ < maxValue))
    println()
    val listMutable = measures("Buffer List Immutable")(list.toBuffer)(_.last)(_.appended(6))(_.remove(0, maxValue - 1))
    listImmutable.foreach(map => assert(map._2 > listMutable(map._1)))
    
  





