package fix

import strawman.collection
import strawman.collection.{ immutable, mutable }
import strawman.collection.immutable.Map
object Collectionstrawman_v0_Map {
  val xs: Map[Int, String] = Map(1 -> "1", 2 -> "2", 3 -> "3")
  val ys: collection.Map[Int, String] = collection.Map.empty
  val ws: immutable.Map[Int, String] = immutable.Map.empty
  val zs: mutable.Map[Int, String] = mutable.Map.empty
}
