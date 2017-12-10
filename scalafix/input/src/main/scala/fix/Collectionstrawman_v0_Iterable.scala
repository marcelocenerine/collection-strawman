/*
rule = "scala:fix.Collectionstrawman_v0"
 */
package fix

import scala.collection.{immutable, mutable}
object Collectionstrawman_v0_Iterable {
  val xs: Iterable[Int] = Iterable(1, 2, 3)
  val ys: collection.Iterable[Int] = collection.Iterable(1, 2, 3)
  val ws: immutable.Iterable[Int] = immutable.Iterable(1, 2, 3)
  val zs: mutable.Iterable[Int] = mutable.Iterable(1, 2, 3)
}
