package fix

import scala.Predef.{ augmentString => _, genericArrayOps => _, intArrayOps => _, wrapString => _, _ }
import strawman.collection.{ arrayToArrayOps, stringToStringOps }
object Collectionstrawman_v0_ArrayAndString {
  def foo(xs: Array[Int], ys: String): Unit = {
    xs.map(x => x + 1)
    ys.map(c => c.toUpper).map(_.toLower)
  }
}
