package fix

import scala.Predef.{ augmentString => _, genericArrayOps => _, intArrayOps => _, wrapString => _, _ }
import strawman.collection
import strawman.collection.{ BitSet, BufferedIterator, IndexedSeq, Iterable, Iterator, LinearSeq, Seq, SortedMap, SortedSet, arrayToArrayOps, concurrent, immutable, mutable, stringToStringOps }
import strawman.collection.concurrent.TrieMap
import strawman.collection.immutable.{ ::, LazyList, List, ListMap, ListSet, Map, Nil, Range, Set, Vector }
import strawman.collection.immutable.LazyList.#::
import strawman.collection.mutable.{ ArrayBuffer, LinkedHashMap, LinkedHashSet, ListBuffer, StringBuilder }
object Collectionstrawman_v0_List {
  List(1, 2, 3)
  1 :: 2 :: 3 :: Nil
  val isEmpty: List[_] => Boolean = {
    case Nil     => true
    case x :: xs => false
  }
}

object Collectionstrawman_v0_Stream {
  LazyList(1, 2, 3)
  1 #:: 2 #:: 3 #:: LazyList.Empty
  val isEmpty: LazyList[_] => Boolean = {
    case LazyList.Empty => true
    case x #:: xs     => false
  }
}

object Collectionstrawman_v0_Vector {
  val xs: Vector[Int] = Vector(1, 2, 3)
}

object Collectionstrawman_v0_Seq {
  val xs: Seq[Int] = Seq(1, 2, 3)
  val ys: collection.Seq[Int] = collection.Seq(1, 2, 3)
  val ws: immutable.Seq[Int] = immutable.Seq(1, 2, 3)
  val zs: mutable.Seq[Int] = mutable.Seq(1, 2, 3)
}

object Collectionstrawman_v0_IndexedSeq {
  val xs: IndexedSeq[Int] = IndexedSeq(1, 2, 3)
  val ys: collection.IndexedSeq[Int] = collection.IndexedSeq(1, 2, 3)
  val ws: immutable.IndexedSeq[Int] = immutable.IndexedSeq(1, 2, 3)
  //val zs: mutable.IndexedSeq[Int] = mutable.IndexedSeq(1, 2, 3)
}

object Collectionstrawman_v0_LinearSeq {
  val xs: LinearSeq[Int] = ???
  val ys: immutable.LinearSeq[Int] = ???
  //val zs: mutable.LinearSeq[Int] = ???
}

object Collectionstrawman_v0_ListBuffer {
  val xs: ListBuffer[Int] = ListBuffer.empty
}

object Collectionstrawman_v0_ListMap {
  val xs: ListMap[Int, String] = ListMap.empty
}

object Collectionstrawman_v0_ListSet {
  val xs: ListSet[Int] = ListSet(1, 2, 3)
}

object Collectionstrawman_v0_Set {
  val xs: Set[Int] = Set(1, 2, 3)
  val ys: collection.Set[Int] = collection.Set(1, 2, 3)
  val ws: immutable.Set[Int] = immutable.Set(1, 2, 3)
  val zs: mutable.Set[Int] = mutable.Set(1, 2, 3)
}

object Collectionstrawman_v0_HashSet {
  val ws: immutable.HashSet[Int] = immutable.HashSet(1, 2, 3)
  val zs: mutable.HashSet[Int] = mutable.HashSet(1, 2, 3)
}

object Collectionstrawman_v0_Map {
  val xs: Map[Int, String] = Map(1 -> "1", 2 -> "2", 3 -> "3")
  val ys: collection.Map[Int, String] = collection.Map.empty
  val ws: immutable.Map[Int, String] = immutable.Map.empty
  val zs: mutable.Map[Int, String] = mutable.Map.empty
}

object Collectionstrawman_v0_HashMap {
  val ws: immutable.HashMap[Int, String] = immutable.HashMap.empty
  val zs: mutable.HashMap[Int, String] = mutable.HashMap.empty
}

object Collectionstrawman_v0_Traversable {
  val xs: Iterable[Int] = Iterable(1, 2, 3)
  val ys: collection.Iterable[Int] = collection.Iterable(1, 2, 3)
  val ws: immutable.Iterable[Int] = immutable.Iterable(1, 2, 3)
  val zs: mutable.Iterable[Int] = mutable.Iterable(1, 2, 3)
}

object Collectionstrawman_v0_Iterator {
  val xs: Iterator[Int] = Iterator(1, 2, 3)
  val ys: collection.Iterator[Int] = collection.Iterator(1, 2, 3)
}

object Collectionstrawman_v0_ArrayBuffer {
  val xs: ArrayBuffer[Int] = ArrayBuffer(1, 2, 3)
}

object Collectionstrawman_v0_ArrayAndString {
  def foo(xs: Array[Int], ys: String): Unit = {
    xs.map(x => x + 1)
    ys.map(c => c.toUpper).map(_.toLower)
  }
}

object Collectionstrawman_v0_Range {
  Range.inclusive(1, 10).map(_ + 2)
  Range(0, 10).map(_ + 3)
}

object Collectionstrawman_v0_Iterable {
  val xs: Iterable[Int] = Iterable(1, 2, 3)
  val ys: collection.Iterable[Int] = collection.Iterable(1, 2, 3)
  val ws: immutable.Iterable[Int] = immutable.Iterable(1, 2, 3)
  val zs: mutable.Iterable[Int] = mutable.Iterable(1, 2, 3)
}

object Collectionstrawman_v0_BufferedIterator {
  val xs: BufferedIterator[Int] = Iterator.empty.buffered
  val ys: collection.BufferedIterator[Int] = Iterator.empty.buffered
}

object Collectionstrawman_v0_StringBuilder {
  val xs: StringBuilder = new StringBuilder()
  val ys: mutable.StringBuilder = new mutable.StringBuilder()
}

object Collectionstrawman_v0_BitSet {
  val xs: BitSet = ???
  val ys: immutable.BitSet = immutable.BitSet.empty
  val zs: mutable.BitSet = mutable.BitSet.empty
}

object Collectionstrawman_v0_TreeSet {
  val xs: immutable.TreeSet[Int] = immutable.TreeSet.empty
  val ys: mutable.TreeSet[Int] = mutable.TreeSet.empty
}

object Collectionstrawman_v0_TreeMap {
  val xs: immutable.TreeMap[Int, String] = immutable.TreeMap.empty
  val ys: mutable.TreeMap[Int, String] = mutable.TreeMap.empty
}

object Collectionstrawman_v0_SortedSet {
  val xs: SortedSet[Int] = SortedSet.empty
  val ys: immutable.SortedSet[Int] = immutable.SortedSet.empty
  val zs: mutable.SortedSet[Int] = mutable.SortedSet.empty
}

object Collectionstrawman_v0_SortedMap {
  val xs: SortedMap[Int, String] = SortedMap.empty
  val ys: immutable.SortedMap[Int, String] = immutable.SortedMap.empty
  val zs: mutable.SortedMap[Int, String] = mutable.SortedMap.empty
}

object Collectionstrawman_v0_LinkedHashSet {
  val xs: LinkedHashSet[Int] = LinkedHashSet.empty
}

object Collectionstrawman_v0_LinkedHashMap {
  val xs: LinkedHashMap[Int, String] = LinkedHashMap.empty
}

object Collectionstrawman_v0_TrieMap {
  val xs: concurrent.Map[Int, String] = TrieMap.empty
}