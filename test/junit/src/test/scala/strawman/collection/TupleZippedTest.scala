package strawman.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.immutable.List

import scala.language.implicitConversions

@RunWith(classOf[JUnit4])
class TupleZippedTest {

  private val ws = List(1, 2, 3)
  private val xs = List(1, 2, 3, 4, 5, 6)
  private val ys = List("a", "b", "c", "d", "e", "f")
  private val zs = List(true, false, true, false, true, false)
  private val zipped2 = ws lazyZip xs
  private val zipped3 = zipped2 lazyZip ys
  private val zipped4 = zipped3 lazyZip zs

  @Test
  def tuple2Zipped_map(): Unit = {
    val res: List[(Int, Int)] = zipped2.map((a, b) => (a, b))

    assertEquals(List((1, 1), (2, 2), (3, 3)), res)
  }

  @Test
  def tuple2Zipped_flatMap(): Unit = {
    val res: List[(Int, Int)] = zipped2.flatMap((a, b) => List((a, b)))

    assertEquals(List((1, 1), (2, 2), (3, 3)), res)
  }

  @Test
  def tuple2Zipped_filter(): Unit = {
    val res: (List[Int], List[Int]) = zipped2.filter((a, _) => a % 2 == 0)

    assertEquals((List(2), List(2)), res)
  }

  @Test
  def tuple2Zipped_exists(): Unit = {
    assertTrue(zipped2.exists((a, b) => a + b > 5))
    assertFalse(zipped2.exists((a, b) => a + b < 0))
  }

  @Test
  def tuple2Zipped_forall(): Unit = {
    assertTrue(zipped2.forall((a, b) => a + b > 0))
    assertFalse(zipped2.forall((a, b) => a + b > 2))
  }

  @Test
  def tuple2Zipped_foreach(): Unit = {
    var res = ""
    zipped2.foreach((a, b) => res += s"[$a,$b]")

    assertEquals("[1,1][2,2][3,3]", res)
  }

  @Test
  def tuple2Zipped_toIterable(): Unit = {
    val iter: Iterable[(Int, Int)] = zipped2

    assertEquals(List((1, 1), (2, 2), (3, 3)), iter.to(List))
  }

  @Test
  def tuple3Zipped_map(): Unit = {
    val res: List[(Int, Int, String)] = zipped3.map((a, b, c) => (a, b, c))

    assertEquals(List((1, 1, "a"), (2, 2, "b"), (3, 3, "c")), res)
  }

  @Test
  def tuple3Zipped_flatMap(): Unit = {
    val res: List[(Int, Int, String)] = zipped3.flatMap((a, b, c) => List((a, b, c)))

    assertEquals(List((1, 1, "a"), (2, 2, "b"), (3, 3, "c")), res)
  }

  @Test
  def tuple3Zipped_filter(): Unit = {
    val res: (List[Int], List[Int], List[String]) = zipped3.filter((a, _, _) => a % 2 != 0)

    assertEquals((List(1, 3), List(1, 3), List("a", "c")), res)
  }

  @Test
  def tuple3Zipped_exists(): Unit = {
    assertTrue(zipped3.exists((a, b, _) => a + b > 5))
    assertFalse(zipped3.exists((a, b, _) => a + b < 0))
  }

  @Test
  def tuple3Zipped_forall(): Unit = {
    assertTrue(zipped3.forall((a, b, _) => (a + b) % 2 == 0))
    assertFalse(zipped3.forall((a, b, _) => a + b > 5))
  }

  @Test
  def tuple3Zipped_foreach(): Unit = {
    var res = ""
    zipped3.foreach((a, b, c) => res += s"[$a,$b,$c]")

    assertEquals("[1,1,a][2,2,b][3,3,c]", res)
  }

  @Test
  def tuple3Zipped_toIterable(): Unit = {
    val iter: Iterable[(Int, Int, String)] = zipped3

    assertEquals(List((1, 1, "a"), (2, 2, "b"), (3, 3, "c")), iter.to(List))
  }

  @Test
  def tuple4Zipped_map(): Unit = {
    val res: List[(Int, Int, String, Boolean)] = zipped4.map((a, b, c, d) => (a, b, c, d))

    assertEquals(List((1, 1, "a", true), (2, 2, "b", false), (3, 3, "c", true)), res)
  }

  @Test
  def tuple4Zipped_flatMap(): Unit = {
    val res: List[(Int, Int, String, Boolean)] = zipped4.flatMap((a, b, c, d) => List((a, b, c, d)))

    assertEquals(List((1, 1, "a", true), (2, 2, "b", false), (3, 3, "c", true)), res)
  }

  @Test
  def tuple4Zipped_filter(): Unit = {
    val res: (List[Int], List[Int], List[String], List[Boolean]) = zipped4.filter((_, _, _, d) => d)

    assertEquals((List(1, 3), List(1, 3), List("a", "c"), List(true, true)), res)
  }

  @Test
  def tuple4Zipped_exists(): Unit = {
    assertTrue(zipped4.exists((a, b, c, d) => a + b > 5 && !c.isEmpty && d))
    assertFalse(zipped4.exists((a, b, c, d) => a + b > 5 && !c.isEmpty && !d))
  }

  @Test
  def tuple4Zipped_forall(): Unit = {
    assertTrue(zipped4.forall((a, b, _, _) => (a + b) % 2 == 0))
    assertFalse(zipped4.forall((a, b, _, d) => a + b > 0 && d))
  }

  @Test
  def tuple4Zipped_foreach(): Unit = {
    var res = ""
    zipped4.foreach((a, b, c, d) => res += s"[$a,$b,$c,$d]")

    assertEquals("[1,1,a,true][2,2,b,false][3,3,c,true]", res)
  }

  @Test
  def tuple4Zipped_toIterable(): Unit = {
    val iter: Iterable[(Int, Int, String, Boolean)] = zipped4

    assertEquals(List((1, 1, "a", true), (2, 2, "b", false), (3, 3, "c", true)), iter.to(List))
  }
}
