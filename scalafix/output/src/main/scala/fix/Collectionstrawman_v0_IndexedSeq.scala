package fix

import strawman.collection
import strawman.collection.{ IndexedSeq, immutable }
object Collectionstrawman_v0_IndexedSeq {
  val xs: IndexedSeq[Int] = IndexedSeq(1, 2, 3)
  val ys: collection.IndexedSeq[Int] = collection.IndexedSeq(1, 2, 3)
  val ws: immutable.IndexedSeq[Int] = immutable.IndexedSeq(1, 2, 3)
  //val zs: mutable.IndexedSeq[Int] = mutable.IndexedSeq(1, 2, 3)
}
