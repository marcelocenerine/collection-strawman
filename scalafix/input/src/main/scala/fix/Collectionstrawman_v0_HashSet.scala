/*
rule = "scala:fix.Collectionstrawman_v0"
 */
package fix

import scala.collection.{immutable, mutable}
object Collectionstrawman_v0_HashSet {
  val ws: immutable.HashSet[Int] = immutable.HashSet(1, 2, 3)
  val zs: mutable.HashSet[Int] = mutable.HashSet(1, 2, 3)
}
