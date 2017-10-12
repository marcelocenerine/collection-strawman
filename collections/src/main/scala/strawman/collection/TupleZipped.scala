package strawman.collection

import scala.{Boolean, StringContext, Unit}
import scala.language.implicitConversions

final class Tuple2Zipped[El1, C1 <: Iterable[El1], El2, C2 <: Iterable[El2]] private[collection](coll1: C1, coll2: C2) {

  def lazyZip[B, C3 <: Iterable[B]](that: C3): Tuple3Zipped[El1, C1, El2, C2, B, C3] = new Tuple3Zipped(coll1, coll2, that)

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

  def filter[C, D](p: (El1, El2) => Boolean)(implicit bf1: BuildFrom[C1, El1, C], bf2: BuildFrom[C2, El2, D]): (C, D) = {
    val res1 = bf1.fromSpecificIterable(coll1)(new View[El1] {
      def iterator() = {
        val it1 = coll1.iterator()
        val it2 = coll2.iterator()
        it1.filter(e => it2.hasNext && p(e, it2.next()))
      }
    })
    val res2 = bf2.fromSpecificIterable(coll2)(new View[El2] {
      def iterator() = {
        val it1 = coll1.iterator()
        val it2 = coll2.iterator()
        it2.filter(e => it1.hasNext && p(it1.next(), e))
      }
    })

    (res1, res2)
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
    new View[(El1, El2)] {
      def iterator() = zipped2.iterator()
    }
}


final class Tuple3Zipped[El1, C1 <: Iterable[El1],
                         El2, C2 <: Iterable[El2],
                         El3, C3 <: Iterable[El3]] private[collection](coll1: C1, coll2: C2, coll3: C3) {

  def lazyZip[B, C4 <: Iterable[B]](that: C4): Tuple4Zipped[El1, C1, El2, C2, El3, C3, B, C4] = new Tuple4Zipped(coll1, coll2, coll3, that)

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

  def filter[C, D, E](p: (El1, El2, El3) => Boolean)(implicit bf1: BuildFrom[C1, El1, C],
                                                              bf2: BuildFrom[C2, El2, D],
                                                              bf3: BuildFrom[C3, El3, E]): (C, D, E) = {
    val res1 = bf1.fromSpecificIterable(coll1)(new View[El1] {
      def iterator() = {
        val it1 = coll1.iterator()
        val it2 = coll2.iterator()
        val it3 = coll3.iterator()
        it1.filter(e => it2.hasNext && it3.hasNext && p(e, it2.next(), it3.next()))
      }
    })
    val res2 = bf2.fromSpecificIterable(coll2)(new View[El2] {
      def iterator() = {
        val it1 = coll1.iterator()
        val it2 = coll2.iterator()
        val it3 = coll3.iterator()
        it2.filter(e => it1.hasNext && it3.hasNext && p(it1.next(), e, it3.next()))
      }
    })
    val res3 = bf3.fromSpecificIterable(coll3)(new View[El3] {
      def iterator() = {
        val it1 = coll1.iterator()
        val it2 = coll2.iterator()
        val it3 = coll3.iterator()
        it3.filter(e => it1.hasNext && it2.hasNext && p(it1.next(), it2.next(), e))
      }
    })

    (res1, res2, res3)
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
    new View[(El1, El2, El3)] {
      def iterator() = zipped3.iterator()
    }
}




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

  def filter[C, D, E, F](p: (El1, El2, El3, El4) => Boolean)(implicit bf1: BuildFrom[C1, El1, C],
                                                                      bf2: BuildFrom[C2, El2, D],
                                                                      bf3: BuildFrom[C3, El3, E],
                                                                      bf4: BuildFrom[C4, El4, F]): (C, D, E, F) = {
    val res1 = bf1.fromSpecificIterable(coll1)(new View[El1] {
      def iterator() = {
        val it1 = coll1.iterator()
        val it2 = coll2.iterator()
        val it3 = coll3.iterator()
        val it4 = coll4.iterator()
        it1.filter(e => it2.hasNext && it3.hasNext && it4.hasNext && p(e, it2.next(), it3.next(), it4.next()))
      }
    })
    val res2 = bf2.fromSpecificIterable(coll2)(new View[El2] {
      def iterator() = {
        val it1 = coll1.iterator()
        val it2 = coll2.iterator()
        val it3 = coll3.iterator()
        val it4 = coll4.iterator()
        it2.filter(e => it1.hasNext && it3.hasNext && it4.hasNext && p(it1.next(), e, it3.next(), it4.next()))
      }
    })
    val res3 = bf3.fromSpecificIterable(coll3)(new View[El3] {
      def iterator() = {
        val it1 = coll1.iterator()
        val it2 = coll2.iterator()
        val it3 = coll3.iterator()
        val it4 = coll4.iterator()
        it3.filter(e => it1.hasNext && it2.hasNext && it4.hasNext && p(it1.next(), it2.next(), e, it4.next()))
      }
    })
    val res4 = bf4.fromSpecificIterable(coll4)(new View[El4] {
      def iterator() = {
        val it1 = coll1.iterator()
        val it2 = coll2.iterator()
        val it3 = coll3.iterator()
        val it4 = coll4.iterator()
        it4.filter(e => it1.hasNext && it2.hasNext && it3.hasNext && p(it1.next(), it2.next(), it3.next(), e))
      }
    })

    (res1, res2, res3, res4)
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
    new View[(El1, El2, El3, El4)] {
      def iterator() = zipped4.iterator()
    }
}