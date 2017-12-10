package fix

import strawman.collection
import strawman.collection.{ Seq, immutable, mutable }
object Collectionstrawman_v0_Seq {
  val xs: Seq[Int] = Seq(1, 2, 3)
  val ys: collection.Seq[Int] = collection.Seq(1, 2, 3)
  val ws: immutable.Seq[Int] = immutable.Seq(1, 2, 3)
  val zs: mutable.Seq[Int] = mutable.Seq(1, 2, 3)
}
