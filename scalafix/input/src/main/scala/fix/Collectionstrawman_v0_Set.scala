/*
rule = "scala:fix.Collectionstrawman_v0"
 */
package fix

import scala.collection.{immutable, mutable}
object Collectionstrawman_v0_Set {
  val xs: Set[Int] = Set(1, 2, 3)
  val ys: collection.Set[Int] = collection.Set(1, 2, 3)
  val ws: immutable.Set[Int] = immutable.Set(1, 2, 3)
  val zs: mutable.Set[Int] = mutable.Set(1, 2, 3)
}
