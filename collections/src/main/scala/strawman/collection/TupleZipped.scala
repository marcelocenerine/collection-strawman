package strawman.collection

import scala.{Boolean, StringContext, Unit}

final class Tuple2Zipped[+El1, +El2] private[collection](coll1: Iterable[El1], coll2: Iterable[El2]) {

  def lazyZip[B](that: Iterable[B]): Tuple3Zipped[El1, El2, B] = new Tuple3Zipped(coll1, coll2, that)

  def map[B](f: (El1, El2) => B): Iterable[B] = {
    val b = coll1.iterableFactory.newBuilder[B]()
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()

    while (elems1.hasNext && elems2.hasNext) b += f(elems1.next(), elems2.next())

    b.result()
  }

  def flatMap[B](f: (El1, El2) => Iterable[B]): Iterable[B] = {
    val b = coll1.iterableFactory.newBuilder[B]()
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()

    while (elems1.hasNext && elems2.hasNext) b ++= f(elems1.next(), elems2.next())

    b.result()
  }

  def filter(p: (El1, El2) => Boolean): (Iterable[El1], Iterable[El2]) = {
    val b1 = coll1.iterableFactory.newBuilder[El1]()
    val b2 = coll2.iterableFactory.newBuilder[El2]()
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
  implicit def tuple2ZippedToIterable[El1, El2](zipped2: Tuple2Zipped[El1, El2]): Iterable[(El1, El2)] = new View[(El1, El2)] {
    override def iterator(): Iterator[(El1, El2)] = zipped2.iterator()
  }
}



final class Tuple3Zipped[+El1, +El2, +El3] private[collection](coll1: Iterable[El1], coll2: Iterable[El2], coll3: Iterable[El3]) {

  def lazyZip[B](that: Iterable[B]): Tuple4Zipped[El1, El2, El3, B] = new Tuple4Zipped(coll1, coll2, coll3, that)

  def map[B](f: (El1, El2, El3) => B): Iterable[B] = {
    val b = coll1.iterableFactory.newBuilder[B]()
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext)
      b += f(elems1.next(), elems2.next(), elems3.next())

    b.result()
  }

  def flatMap[B](f: (El1, El2, El3) => Iterable[B]): Iterable[B] = {
    val b = coll1.iterableFactory.newBuilder[B]()
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext)
      b ++= f(elems1.next(), elems2.next(), elems3.next())

    b.result()
  }

  def filter(p: (El1, El2, El3) => Boolean): (Iterable[El1], Iterable[El2], Iterable[El3]) = {
    val b1 = coll1.iterableFactory.newBuilder[El1]()
    val b2 = coll2.iterableFactory.newBuilder[El2]()
    val b3 = coll3.iterableFactory.newBuilder[El3]()
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
  implicit def tuple3ZippedToIterable[El1, El2, El3](zipped3: Tuple3Zipped[El1, El2, El3]): Iterable[(El1, El2, El3)] =
    new View[(El1, El2, El3)] {
      override def iterator(): Iterator[(El1, El2, El3)] = zipped3.iterator()
  }
}




final class Tuple4Zipped[+El1, +El2, +El3, +El4] private[collection](coll1: Iterable[El1],
                                                                     coll2: Iterable[El2],
                                                                     coll3: Iterable[El3],
                                                                     coll4: Iterable[El4]) {

  def map[B](f: (El1, El2, El3, El4) => B): Iterable[B] = {
    val b = coll1.iterableFactory.newBuilder[B]()
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()
    val elems4 = coll4.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext)
      b += f(elems1.next(), elems2.next(), elems3.next(), elems4.next())

    b.result()
  }

  def flatMap[B](f: (El1, El2, El3, El4) => Iterable[B]): Iterable[B] = {
    val b = coll1.iterableFactory.newBuilder[B]()
    val elems1 = coll1.iterator()
    val elems2 = coll2.iterator()
    val elems3 = coll3.iterator()
    val elems4 = coll4.iterator()

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext)
      b ++= f(elems1.next(), elems2.next(), elems3.next(), elems4.next())

    b.result()
  }

  def filter(p: (El1, El2, El3, El4) => Boolean): (Iterable[El1], Iterable[El2], Iterable[El3], Iterable[El4]) = {
    val b1 = coll1.iterableFactory.newBuilder[El1]()
    val b2 = coll2.iterableFactory.newBuilder[El2]()
    val b3 = coll3.iterableFactory.newBuilder[El3]()
    val b4 = coll4.iterableFactory.newBuilder[El4]()
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
  implicit def tuple4ZippedToIterable[El1, El2, El3, El4](zipped4: Tuple4Zipped[El1, El2, El3, El4]): Iterable[(El1, El2, El3, El4)] =
    new View[(El1, El2, El3, El4)] {
      override def iterator(): Iterator[(El1, El2, El3, El4)] = zipped4.iterator()
  }
}