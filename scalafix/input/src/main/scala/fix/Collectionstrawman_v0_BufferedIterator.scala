/*
rule = "scala:fix.Collectionstrawman_v0"
 */
package fix

object Collectionstrawman_v0_BufferedIterator {
  val xs: BufferedIterator[Int] = Iterator.empty.buffered
  val ys: collection.BufferedIterator[Int] = Iterator.empty.buffered
}
