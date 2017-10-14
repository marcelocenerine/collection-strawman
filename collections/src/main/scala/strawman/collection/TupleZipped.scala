package strawman.collection

import scala.{Boolean, StringContext, Unit}
import scala.language.implicitConversions

/** Decorator representing lazily zipped tuples of arity 2. */
final class Tuple2Zipped[El1, C1 <: Iterable[El1], El2, C2 <: Iterable[El2]] private[collection](coll1: C1, coll2: C2) {

  /** Zips `that` iterable collection with an already lazily zipped `Tuple2Zipped`. The elements in each collection are
    * not consumed until a strict operation is invoked on the returned `Tuple3Zipped` decorator.
    *
    * @param that the iterable providing the third element of each eventual tuple
    * @tparam B the type of the third element in each eventual tuple
    * @tparam C3 the type of `that` iterable
    * @return a decorator `Tuple3Zipped` that allows strict operations to be performed on the lazily evaluated tuples or
    *         chained calls to `lazyZip`. Implicit conversion to `Iterable[(El1, El2, B)]` is also supported.
    */
  def lazyZip[B, C3[X] <: Iterable[X]](that: C3[B]): Tuple3Zipped[El1, C1, El2, C2, B, C3[B]] = new Tuple3Zipped(coll1, coll2, that)

  def map[B, C](f: (El1, El2) => B)(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecificIterable(coll1)(new View[B] {
      def iterator() = new Iterator[B] {
        private val elems1 = coll1.iterator()
        private val elems2 = coll2.iterator()
        def hasNext = elems1.hasNext && elems2.hasNext
        def next() = f(elems1.next(), elems2.next())
      }
    })
  }

  def flatMap[B, C](f: (El1, El2) => Iterable[B])(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecificIterable(coll1)(new View[B] {
      def iterator() = new Iterator[B] {
        private val elems1 = coll1.iterator()
        private val elems2 = coll2.iterator()
        private var myCurrent: Iterator[B] = Iterator.empty
        private def current = {
          while (!myCurrent.hasNext && elems1.hasNext && elems2.hasNext)
            myCurrent = f(elems1.next(), elems2.next()).iterator()
          myCurrent
        }

        def hasNext = current.hasNext
        def next() = current.next()
      }
    })
  }

  def filter[C](p: (El1, El2) => Boolean)(implicit bf: BuildFrom[C1, (El1, El2), C]): C = {
    bf.fromSpecificIterable(coll1)(new View[(El1, El2)] {
      def iterator() = new Iterator[(El1, El2)] {
        private val elems1 = coll1.iterator()
        private val elems2 = coll2.iterator()
        private var myCurrent: (El1, El2) = _
        private def current = {
          while ((myCurrent eq null) && elems1.hasNext && elems2.hasNext) {
            val e1 = elems1.next()
            val e2 = elems2.next()
            if (p(e1, e2)) myCurrent = (e1, e2)
          }
          myCurrent
        }
        def hasNext = current ne null
        def next() = {
          val c = current
          if (c ne null) {
            myCurrent = null
            c
          } else Iterator.empty.next()
        }
      }
    })
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

  private def iterator() = new Iterator[(El1, El2)] {
    private val elems1 = coll1.iterator()
    private val elems2 = coll2.iterator()
    def hasNext = elems1.hasNext && elems2.hasNext
    def next() = (elems1.next(), elems2.next())
  }

  def className = getClass.getName

  override def toString = s"$className($coll1, $coll2)"
}

object Tuple2Zipped {
  implicit def tuple2ZippedToIterable[El1, C1 <: Iterable[El1],
                                      El2, C2 <: Iterable[El2]](zipped2: Tuple2Zipped[El1, C1, El2, C2]): Iterable[(El1, El2)] =
    new View[(El1, El2)] { def iterator() = zipped2.iterator() }
}


/** Decorator representing lazily zipped tuples of arity 3. */
final class Tuple3Zipped[El1, C1 <: Iterable[El1],
                         El2, C2 <: Iterable[El2],
                         El3, C3 <: Iterable[El3]] private[collection](coll1: C1, coll2: C2, coll3: C3) {

  /** Zips `that` iterable collection with an already lazily zipped `Tuple3Zipped`. The elements in each collection are
    * not consumed until a strict operation is invoked on the returned `Tuple4Zipped` decorator.
    *
    * @param that the iterable providing the forth element of each eventual tuple
    * @tparam B the type of the forth element in each eventual tuple
    * @tparam C4 the type of `that` iterable
    * @return a decorator `Tuple4Zipped` that allows strict operations to be performed on the lazily evaluated tuples.
    *         Implicit conversion to `Iterable[(El1, El2, El3, B)]` is also supported.
    */
  def lazyZip[B, C4[X] <: Iterable[X]](that: C4[B]): Tuple4Zipped[El1, C1, El2, C2, El3, C3, B, C4[B]] = new Tuple4Zipped(coll1, coll2, coll3, that)

  def map[B, C](f: (El1, El2, El3) => B)(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecificIterable(coll1)(new View[B] {
      def iterator() = new Iterator[B] {
        private val elems1 = coll1.iterator()
        private val elems2 = coll2.iterator()
        private val elems3 = coll3.iterator()
        def hasNext = elems1.hasNext && elems2.hasNext && elems3.hasNext
        def next() = f(elems1.next(), elems2.next(), elems3.next())
      }
    })
  }

  def flatMap[B, C](f: (El1, El2, El3) => Iterable[B])(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecificIterable(coll1)(new View[B] {
      def iterator() = new Iterator[B] {
        private val elems1 = coll1.iterator()
        private val elems2 = coll2.iterator()
        private val elems3 = coll3.iterator()
        private var myCurrent: Iterator[B] = Iterator.empty
        private def current = {
          while (!myCurrent.hasNext && elems1.hasNext && elems2.hasNext && elems3.hasNext)
            myCurrent = f(elems1.next(), elems2.next(), elems3.next()).iterator()
          myCurrent
        }

        def hasNext = current.hasNext
        def next() = current.next()
      }
    })
  }

  def filter[C](p: (El1, El2, El3) => Boolean)(implicit bf: BuildFrom[C1, (El1, El2, El3), C]): C = {
    bf.fromSpecificIterable(coll1)(new View[(El1, El2, El3)] {
      def iterator() = new Iterator[(El1, El2, El3)] {
        private val elems1 = coll1.iterator()
        private val elems2 = coll2.iterator()
        private val elems3 = coll3.iterator()
        private var myCurrent: (El1, El2, El3) = _
        private def current = {
          while ((myCurrent eq null) && elems1.hasNext && elems2.hasNext && elems3.hasNext) {
            val e1 = elems1.next()
            val e2 = elems2.next()
            val e3 = elems3.next()
            if (p(e1, e2, e3)) myCurrent = (e1, e2, e3)
          }
          myCurrent
        }
        def hasNext = current ne null
        def next() = {
          val c = current
          if (c ne null) {
            myCurrent = null
            c
          } else Iterator.empty.next()
        }
      }
    })
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

  private def iterator() = new Iterator[(El1, El2, El3)] {
    private val elems1 = coll1.iterator()
    private val elems2 = coll2.iterator()
    private val elems3 = coll3.iterator()
    def hasNext = elems1.hasNext && elems2.hasNext && elems3.hasNext
    def next() = (elems1.next(), elems2.next(), elems3.next())
  }

  def className = getClass.getName

  override def toString = s"$className($coll1, $coll2, $coll3)"
}

object Tuple3Zipped {
  implicit def tuple3ZippedToIterable[El1, C1 <: Iterable[El1],
                                      El2, C2 <: Iterable[El2],
                                      El3, C3 <: Iterable[El3]](zipped3: Tuple3Zipped[El1, C1, El2, C2, El3, C3]): Iterable[(El1, El2, El3)] =
    new View[(El1, El2, El3)] { def iterator() = zipped3.iterator() }
}



/** Decorator representing lazily zipped tuples of arity 4. */
final class Tuple4Zipped[El1, C1 <: Iterable[El1],
                         El2, C2 <: Iterable[El2],
                         El3, C3 <: Iterable[El3],
                         El4, C4 <: Iterable[El4]] private[collection](coll1: C1, coll2: C2, coll3: C3, coll4: C4) {

  def map[B, C](f: (El1, El2, El3, El4) => B)(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecificIterable(coll1)(new View[B] {
      def iterator() = new Iterator[B] {
        private val elems1 = coll1.iterator()
        private val elems2 = coll2.iterator()
        private val elems3 = coll3.iterator()
        private val elems4 = coll4.iterator()
        def hasNext = elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext
        def next() = f(elems1.next(), elems2.next(), elems3.next(), elems4.next())
      }
    })
  }

  def flatMap[B, C](f: (El1, El2, El3, El4) => Iterable[B])(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecificIterable(coll1)(new View[B] {
      def iterator() = new Iterator[B] {
        private val elems1 = coll1.iterator()
        private val elems2 = coll2.iterator()
        private val elems3 = coll3.iterator()
        private val elems4 = coll4.iterator()
        private var myCurrent: Iterator[B] = Iterator.empty
        private def current = {
          while (!myCurrent.hasNext && elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext)
            myCurrent = f(elems1.next(), elems2.next(), elems3.next(), elems4.next()).iterator()
          myCurrent
        }

        def hasNext = current.hasNext
        def next() = current.next()
      }
    })
  }

  def filter[C](p: (El1, El2, El3, El4) => Boolean)(implicit bf: BuildFrom[C1, (El1, El2, El3, El4), C]): C = {
    bf.fromSpecificIterable(coll1)(new View[(El1, El2, El3, El4)] {
      def iterator() = new Iterator[(El1, El2, El3, El4)] {
        private val elems1 = coll1.iterator()
        private val elems2 = coll2.iterator()
        private val elems3 = coll3.iterator()
        private val elems4 = coll4.iterator()
        private var myCurrent: (El1, El2, El3, El4) = _
        private def current = {
          while ((myCurrent eq null) && elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext) {
            val e1 = elems1.next()
            val e2 = elems2.next()
            val e3 = elems3.next()
            val e4 = elems4.next()
            if (p(e1, e2, e3, e4)) myCurrent = (e1, e2, e3, e4)
          }
          myCurrent
        }
        def hasNext = current ne null
        def next() = {
          val c = current
          if (c ne null) {
            myCurrent = null
            c
          } else Iterator.empty.next()
        }
      }
    })
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

  private def iterator() = new Iterator[(El1, El2, El3, El4)] {
    private val elems1 = coll1.iterator()
    private val elems2 = coll2.iterator()
    private val elems3 = coll3.iterator()
    private val elems4 = coll4.iterator()
    def hasNext = elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext
    def next() = (elems1.next(), elems2.next(), elems3.next(), elems4.next())
  }

  def className = getClass.getName

  override def toString = s"$className($coll1, $coll2, $coll3, $coll4)"
}

object Tuple4Zipped {
  implicit def tuple4ZippedToIterable[El1, C1 <: Iterable[El1],
                                      El2, C2 <: Iterable[El2],
                                      El3, C3 <: Iterable[El3],
                                      El4, C4 <: Iterable[El4]](zipped4: Tuple4Zipped[El1, C1, El2, C2, El3, C3, El4, C4]): Iterable[(El1, El2, El3, El4)] =
    new View[(El1, El2, El3, El4)] { def iterator() = zipped4.iterator() }
}