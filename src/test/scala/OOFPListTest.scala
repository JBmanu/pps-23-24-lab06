import ex1.List
import org.junit.Assert.assertEquals
import org.junit.Test

class OOFPListTest:
  val list = List(1, 2, 3, 4)

  @Test def zipWithValue(): Unit =
    val zipList = list.zipWithValue(10)
    assertEquals(List((1, 10), (2, 10), (3, 10), (4, 10)), zipList)

  @Test def length(): Unit =
    assertEquals(4, list.length())

  @Test def zipWithIndex(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), list.zipWithIndex)

  @Test def partition(): Unit =
    assertEquals((List(2, 4), List(1, 3)), list.partition(_ % 2 == 0))

  @Test def span(): Unit =
    assertEquals((List(1), List(2, 3, 4)), list.span(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), list.span(_ < 3))

  @Test def takeRight(): Unit =
    assertEquals(List(2, 3, 4), list.takeRight(3))

  @Test def collect(): Unit =
    assertEquals(List(3, 5), list.collect { case x if x % 2 == 0 => x + 1 })