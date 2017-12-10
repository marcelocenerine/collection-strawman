package fix

import strawman.collection
import strawman.collection.{ immutable, mutable }
import strawman.collection.immutable.Set
object Collectionstrawman_v0_Set {
  val xs: Set[Int] = Set(1, 2, 3)
  val ys: collection.Set[Int] = collection.Set(1, 2, 3)
  val ws: immutable.Set[Int] = immutable.Set(1, 2, 3)
  val zs: mutable.Set[Int] = mutable.Set(1, 2, 3)
}
