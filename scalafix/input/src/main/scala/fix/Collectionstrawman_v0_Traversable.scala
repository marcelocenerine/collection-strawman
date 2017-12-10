/*
rule = "scala:fix.Collectionstrawman_v0"
 */
package fix

import scala.collection.{immutable, mutable}
object Collectionstrawman_v0_Traversable {
  val xs: Traversable[Int] = Traversable(1, 2, 3)
  val ys: collection.Traversable[Int] = collection.Traversable(1, 2, 3)
  val ws: immutable.Traversable[Int] = immutable.Traversable(1, 2, 3)
  val zs: mutable.Traversable[Int] = mutable.Traversable(1, 2, 3)
}
