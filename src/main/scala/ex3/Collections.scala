package ex3

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  enum Exp:
    case Create
    case Read
    case Update
    case Delete

  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    val format: Long => String = time => "%,d".format(time)
    if (msg.nonEmpty) println(msg + " -- " + format(duration.toNanos) + " nanos = " + format(duration.toMillis) + " ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

  def measures[C, T, U](nameObj: String)
                       (createExp: => C)(readExp: => C => T)(updateExp: => C => U)(deleteExp: => C => U): Map[Exp, MeasurementResults[? >: C & T & U]] =
    println(s"$nameObj")
    val create = measure("[Create]")(createExp)
    val read = measure("[Read]  ")(readExp(create.result))
    val update = measure("[Update]")(updateExp(create.result))
    val delete = measure("[Delete]")(deleteExp(create.result))
    println()
    Map(Exp.Create -> create, Exp.Read -> read, Exp.Update -> update, Exp.Delete -> delete)


@main def checkPerformance: Unit =

  /* Linear sequences: List, ListBuffer */

  /* Indexed sequences: Vector, Array, ArrayBuffer */

  /* Sets */

  /* Maps */

  /* Comparison */
  import PerformanceUtils.*
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  assert(measure("lst last")(lst.last) > measure("vec last")(vec.last))
