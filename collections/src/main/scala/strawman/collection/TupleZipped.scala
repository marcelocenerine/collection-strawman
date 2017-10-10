package strawman.collection

import scala.{Boolean, StringContext, Unit}
import scala.language.implicitConversions

final class Tuple2Zipped[El1, +C1, El2, +C2] private[collection](coll1: C1, coll2: C2)(implicit v1: C1 => Iterable[El1],
                                                                                                v2: C2 => Iterable[El2]) {

  def lazyZip[B, C3](that: C3)(implicit v3: C3 => Iterable[B]): Tuple3Zipped[El1, C1, El2, C2, B, C3] =
    new Tuple3Zipped(coll1, coll2, that)

  def map[B, C](f: (El1, El2) => B)(implicit bf: BuildFrom[C1, B, C]): C = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()

    while (elems1.hasNext && elems2.hasNext) b += f(elems1.next(), elems2.next())

    b.result()
  }

  def flatMap[B, C](f: (El1, El2) => Iterable[B])(implicit bf: BuildFrom[C1, B, C]): C = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()

    while (elems1.hasNext && elems2.hasNext) b ++= f(elems1.next(), elems2.next())

    b.result()
  }

  def filter[C, D](p: (El1, El2) => Boolean)(implicit bf1: BuildFrom[C1, El1, C], bf2: BuildFrom[C2, El2, D]): (C, D) = {
    val b1 = bf1.newBuilder(coll1)
    val b2 = bf2.newBuilder(coll2)
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()

    while (elems1.hasNext && elems2.hasNext) {
      val el1 = elems1.next()
      val el2 = elems2.next()
      if (p(el1, el2)) {
        b1 += el1
        b2 += el2
      }
    }

    (b1.result(), b2.result())
  }

  def exists(p: (El1, El2) => Boolean): Boolean = {
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    var res = false

    while (!res && elems1.hasNext && elems2.hasNext) res = p(elems1.next(), elems2.next())

    res
  }

  def forall(p: (El1, El2) => Boolean): Boolean = !exists((el1, el2) => !p(el1, el2))

  def foreach[U](f: (El1, El2) => U): Unit = {
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()

    while (elems1.hasNext && elems2.hasNext) f(elems1.next(), elems2.next())
  }

  private def iterator(): Iterator[(El1, El2)] = new Iterator[(El1, El2)] {
    private val elems1 = coll1.iterator()
    private val elems2 = coll2.iterator()

    override def hasNext: Boolean = elems1.hasNext && elems2.hasNext
    override def next(): (El1, El2) = (elems1.next(), elems2.next())
  }

  def className = getClass.getName

  override def toString = s"$className($coll1, $coll2)"
}

object Tuple2Zipped {
  implicit def tuple2ZippedToIterable[El1, C1, El2, C2](zipped2: Tuple2Zipped[El1, C1, El2, C2]): Iterable[(El1, El2)] =
    new View[(El1, El2)] {
      override def iterator(): Iterator[(El1, El2)] = zipped2.iterator()
    }
}


final class Tuple3Zipped[El1, +C1, El2, +C2, El3, +C3] private[collection](coll1: C1, coll2: C2, coll3: C3)
                                                                          (implicit v1: C1 => Iterable[El1],
                                                                                    v2: C2 => Iterable[El2],
                                                                                    v3: C3 => Iterable[El3]) {

  def lazyZip[B, C4](that: C4)(implicit v4: C4 => Iterable[B]): Tuple4Zipped[El1, C1, El2, C2, El3, C3, B, C4] =
    new Tuple4Zipped(coll1, coll2, coll3, that)

  def map[B, C](f: (El1, El2, El3) => B)(implicit bf: BuildFrom[C1, B, C]): C = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext)
      b += f(elems1.next(), elems2.next(), elems3.next())

    b.result()
  }

  def flatMap[B, C](f: (El1, El2, El3) => Iterable[B])(implicit bf: BuildFrom[C1, B, C]): C = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext)
      b ++= f(elems1.next(), elems2.next(), elems3.next())

    b.result()
  }

  def filter[C, D, E](p: (El1, El2, El3) => Boolean)(implicit bf1: BuildFrom[C1, El1, C],
                                                              bf2: BuildFrom[C2, El2, D],
                                                              bf3: BuildFrom[C3, El3, E]): (C, D, E) = {
    val b1 = bf1.newBuilder(coll1)
    val b2 = bf2.newBuilder(coll2)
    val b3 = bf3.newBuilder(coll3)
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      val el1 = elems1.next()
      val el2 = elems2.next()
      val el3 = elems3.next()
      if (p(el1, el2, el3)) {
        b1 += el1
        b2 += el2
        b3 += el3
      }
    }

    (b1.result(), b2.result(), b3.result())
  }

  def exists(p: (El1, El2, El3) => Boolean): Boolean = {
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()
    var res = false

    while (!res && elems1.hasNext && elems2.hasNext && elems3.hasNext)
      res = p(elems1.next(), elems2.next(), elems3.next())

    res
  }

  def forall(p: (El1, El2, El3) => Boolean): Boolean = !exists((el1, el2, el3) => !p(el1, el2, el3))

  def foreach[U](f: (El1, El2, El3) => U): Unit = {
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext)
      f(elems1.next(), elems2.next(), elems3.next())
  }

  private def iterator(): Iterator[(El1, El2, El3)] = new Iterator[(El1, El2, El3)] {
    private val elems1 = coll1.iterator()
    private val elems2 = coll2.iterator()
    private val elems3 = coll3.iterator()

    override def hasNext: Boolean = elems1.hasNext && elems2.hasNext && elems3.hasNext
    override def next(): (El1, El2, El3) = (elems1.next(), elems2.next(), elems3.next())
  }

  def className = getClass.getName

  override def toString = s"$className($coll1, $coll2, $coll3)"
}

object Tuple3Zipped {
  implicit def tuple3ZippedToIterable[El1, C1, El2, C2, El3, C3]
    (zipped3: Tuple3Zipped[El1, C1, El2, C2, El3, C3]): Iterable[(El1, El2, El3)] =
    new View[(El1, El2, El3)] {
      override def iterator(): Iterator[(El1, El2, El3)] = zipped3.iterator()
    }
}




final class Tuple4Zipped[El1, +C1, El2, +C2, El3, +C3, El4, +C4] private[collection](coll1: C1, coll2: C2, coll3: C3, coll4: C4)
                                                                                    (implicit v1: C1 => Iterable[El1],
                                                                                              v2: C2 => Iterable[El2],
                                                                                              v3: C3 => Iterable[El3],
                                                                                              v4: C4 => Iterable[El4]) {

  def map[B, C](f: (El1, El2, El3, El4) => B)(implicit bf: BuildFrom[C1, B, C]): C = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()
    val elems4 = coll4.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext)
      b += f(elems1.next(), elems2.next(), elems3.next(), elems4.next())

    b.result()
  }

  def flatMap[B, C](f: (El1, El2, El3, El4) => Iterable[B])(implicit bf: BuildFrom[C1, B, C]): C = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()
    val elems4 = coll4.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext)
      b ++= f(elems1.next(), elems2.next(), elems3.next(), elems4.next())

    b.result()
  }

  def filter[C, D, E, F](p: (El1, El2, El3, El4) => Boolean)(implicit bf1: BuildFrom[C1, El1, C],
                                                                      bf2: BuildFrom[C2, El2, D],
                                                                      bf3: BuildFrom[C3, El3, E],
                                                                      bf4: BuildFrom[C4, El4, F]): (C, D, E, F) = {
    val b1 = bf1.newBuilder(coll1)
    val b2 = bf2.newBuilder(coll2)
    val b3 = bf3.newBuilder(coll3)
    val b4 = bf4.newBuilder(coll4)
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()
    val elems4 = coll4.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext) {
      val el1 = elems1.next()
      val el2 = elems2.next()
      val el3 = elems3.next()
      val el4 = elems4.next()
      if (p(el1, el2, el3, el4)) {
        b1 += el1
        b2 += el2
        b3 += el3
        b4 += el4
      }
    }

    (b1.result(), b2.result(), b3.result(), b4.result())
  }

  def exists(p: (El1, El2, El3, El4) => Boolean): Boolean = {
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()
    val elems4 = coll4.iterator()
    var res = false

    while (!res && elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext)
      res = p(elems1.next(), elems2.next(), elems3.next(), elems4.next())

    res
  }

  def forall(p: (El1, El2, El3, El4) => Boolean): Boolean = !exists((el1, el2, el3, el4) => !p(el1, el2, el3, el4))

  def foreach[U](f: (El1, El2, El3, El4) => U): Unit = {
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()
    val elems4 = coll4.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext)
      f(elems1.next(), elems2.next(), elems3.next(), elems4.next())
  }

  private def iterator(): Iterator[(El1, El2, El3, El4)] = new Iterator[(El1, El2, El3, El4)] {
    private val elems1 = coll1.iterator()
    private val elems2 = coll2.iterator()
    private val elems3 = coll3.iterator()
    private val elems4 = coll4.iterator()

    override def hasNext: Boolean = elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext
    override def next(): (El1, El2, El3, El4) = (elems1.next(), elems2.next(), elems3.next(), elems4.next())
  }

  def className = getClass.getName

  override def toString = s"$className($coll1, $coll2, $coll3, $coll4)"
}

object Tuple4Zipped {
  implicit def tuple4ZippedToIterable[El1, C1, El2, C2, El3, C3, El4, C4]
    (zipped4: Tuple4Zipped[El1, C1, El2, C2, El3, C3, El4, C4]): Iterable[(El1, El2, El3, El4)] =
    new View[(El1, El2, El3, El4)] {
      override def iterator(): Iterator[(El1, El2, El3, El4)] = zipped4.iterator()
    }
}