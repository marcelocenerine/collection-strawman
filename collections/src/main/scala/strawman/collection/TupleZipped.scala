package strawman.collection

import scala.{Boolean, StringContext, Unit}

final class Tuple2Zipped[+A, +B] private[collection](as: Iterable[A], bs: Iterable[B]) {

  def lazyZip[C](that: Iterable[C]): Tuple3Zipped[A, B, C] = new Tuple3Zipped(as, bs, that)

  def map[C](f: (A, B) => C): Iterable[C] = {
    val builder = as.iterableFactory.newBuilder[C]()
    val ait = as.iterator()
    val bit = bs.iterator()

    while (ait.hasNext && bit.hasNext) builder += f(ait.next(), bit.next())

    builder.result()
  }

  def flatMap[C](f: (A, B) => Iterable[C]): Iterable[C] = {
    val builder = as.iterableFactory.newBuilder[C]()
    val ait = as.iterator()
    val bit = bs.iterator()

    while (ait.hasNext && bit.hasNext) builder ++= f(ait.next(), bit.next())

    builder.result()
  }

  def filter(p: (A, B) => Boolean): (Iterable[A], Iterable[B]) = {
    val builder1 = as.iterableFactory.newBuilder[A]()
    val builder2 = bs.iterableFactory.newBuilder[B]()
    val ait = as.iterator()
    val bit = bs.iterator()

    while (ait.hasNext && bit.hasNext) {
      val a = ait.next()
      val b = bit.next()
      if (p(a, b)) {
        builder1 += a
        builder2 += b
      }
    }

    (builder1.result(), builder2.result())
  }

  def exists(p: (A, B) => Boolean): Boolean = {
    val ait = as.iterator()
    val bit = bs.iterator()
    var res = false

    while (!res && ait.hasNext && bit.hasNext) res = p(ait.next(), bit.next())

    res
  }

  def forall(p: (A, B) => Boolean): Boolean = !exists((a, b) => !p(a, b))

  def foreach[U](f: (A, B) => U): Unit = {
    val ait = as.iterator()
    val bit = bs.iterator()

    while (ait.hasNext && bit.hasNext) f(ait.next(), bit.next())
  }

  private def iterator(): Iterator[(A, B)] = new Iterator[(A, B)] {
    val ait = as.iterator()
    val bit = bs.iterator()

    override def hasNext: Boolean = ait.hasNext && bit.hasNext
    override def next(): (A, B) = (ait.next(), bit.next())
  }

  def className = getClass.getName

  override def toString = s"$className($as, $bs)"
}

object Tuple2Zipped {
  implicit def tuple2ZippedToIterable[A, B](zipped2: Tuple2Zipped[A, B]): Iterable[(A, B)] = new View[(A, B)] {
    override def iterator(): Iterator[(A, B)] = zipped2.iterator()
  }
}



final class Tuple3Zipped[+A, +B, +C] private[collection](as: Iterable[A], bs: Iterable[B], cs: Iterable[C]) {

  def lazyZip[D](that: Iterable[D]): Tuple4Zipped[A, B, C, D] = new Tuple4Zipped(as, bs, cs, that)

  def map[D](f: (A, B, C) => D): Iterable[D] = {
    val builder = as.iterableFactory.newBuilder[D]()
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()

    while (ait.hasNext && bit.hasNext && cit.hasNext)
      builder += f(ait.next(), bit.next(), cit.next())

    builder.result()
  }

  def flatMap[D](f: (A, B, C) => Iterable[D]): Iterable[D] = {
    val builder = as.iterableFactory.newBuilder[D]()
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()

    while (ait.hasNext && bit.hasNext && cit.hasNext)
      builder ++= f(ait.next(), bit.next(), cit.next())

    builder.result()
  }

  def filter(p: (A, B, C) => Boolean): (Iterable[A], Iterable[B], Iterable[C]) = {
    val builder1 = as.iterableFactory.newBuilder[A]()
    val builder2 = bs.iterableFactory.newBuilder[B]()
    val builder3 = cs.iterableFactory.newBuilder[C]()
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()

    while (ait.hasNext && bit.hasNext && cit.hasNext) {
      val a = ait.next()
      val b = bit.next()
      val c = cit.next()
      if (p(a, b, c)) {
        builder1 += a
        builder2 += b
        builder3 += c
      }
    }

    (builder1.result(), builder2.result(), builder3.result())
  }

  def exists(p: (A, B, C) => Boolean): Boolean = {
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()
    var res = false

    while (!res && ait.hasNext && bit.hasNext && cit.hasNext)
      res = p(ait.next(), bit.next(), cit.next())

    res
  }

  def forall(p: (A, B, C) => Boolean): Boolean = !exists((a, b, c) => !p(a, b, c))

  def foreach[U](f: (A, B, C) => U): Unit = {
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()

    while (ait.hasNext && bit.hasNext && cit.hasNext)
      f(ait.next(), bit.next(), cit.next())
  }

  private def iterator(): Iterator[(A, B, C)] = new Iterator[(A, B, C)] {
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()

    override def hasNext: Boolean = ait.hasNext && bit.hasNext && cit.hasNext
    override def next(): (A, B, C) = (ait.next(), bit.next(), cit.next())
  }

  def className = getClass.getName

  override def toString = s"$className($as, $bs, $cs)"
}

object Tuple3Zipped {
  implicit def tuple3ZippedToIterable[A, B, C](zipped3: Tuple3Zipped[A, B, C]): Iterable[(A, B, C)] = new View[(A, B, C)] {
    override def iterator(): Iterator[(A, B, C)] = zipped3.iterator()
  }
}




final class Tuple4Zipped[+A, +B, +C, +D] private[collection](as: Iterable[A], bs: Iterable[B], cs: Iterable[C], ds: Iterable[D]) {

  def map[E](f: (A, B, C, D) => E): Iterable[E] = {
    val builder = as.iterableFactory.newBuilder[E]()
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()
    val dit = ds.iterator()

    while (ait.hasNext && bit.hasNext && cit.hasNext && dit.hasNext)
      builder += f(ait.next(), bit.next(), cit.next(), dit.next())

    builder.result()
  }

  def flatMap[E](f: (A, B, C, D) => Iterable[E]): Iterable[E] = {
    val builder = as.iterableFactory.newBuilder[E]()
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()
    val dit = ds.iterator()

    while (ait.hasNext && bit.hasNext && cit.hasNext && dit.hasNext)
      builder ++= f(ait.next(), bit.next(), cit.next(), dit.next())

    builder.result()
  }

  def filter(p: (A, B, C, D) => Boolean): (Iterable[A], Iterable[B], Iterable[C], Iterable[D]) = {
    val builder1 = as.iterableFactory.newBuilder[A]()
    val builder2 = bs.iterableFactory.newBuilder[B]()
    val builder3 = cs.iterableFactory.newBuilder[C]()
    val builder4 = ds.iterableFactory.newBuilder[D]()
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()
    val dit = ds.iterator()

    while (ait.hasNext && bit.hasNext && cit.hasNext && dit.hasNext) {
      val a = ait.next()
      val b = bit.next()
      val c = cit.next()
      val d = dit.next()
      if (p(a, b, c, d)) {
        builder1 += a
        builder2 += b
        builder3 += c
        builder4 += d
      }
    }

    (builder1.result(), builder2.result(), builder3.result(), builder4.result())
  }

  def exists(p: (A, B, C, D) => Boolean): Boolean = {
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()
    val dit = ds.iterator()
    var res = false

    while (!res && ait.hasNext && bit.hasNext && cit.hasNext && dit.hasNext)
      res = p(ait.next(), bit.next(), cit.next(), dit.next())

    res
  }

  def forall(p: (A, B, C, D) => Boolean): Boolean = !exists((a, b, c, d) => !p(a, b, c, d))

  def foreach[U](f: (A, B, C, D) => U): Unit = {
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()
    val dit = ds.iterator()

    while (ait.hasNext && bit.hasNext && cit.hasNext && dit.hasNext)
      f(ait.next(), bit.next(), cit.next(), dit.next())
  }

  private def iterator(): Iterator[(A, B, C, D)] = new Iterator[(A, B, C, D)] {
    val ait = as.iterator()
    val bit = bs.iterator()
    val cit = cs.iterator()
    val dit = ds.iterator()

    override def hasNext: Boolean = ait.hasNext && bit.hasNext && cit.hasNext && dit.hasNext
    override def next(): (A, B, C, D) = (ait.next(), bit.next(), cit.next(), dit.next())
  }

  def className = getClass.getName

  override def toString = s"$className($as, $bs, $cs, $ds)"
}

object Tuple4Zipped {
  implicit def tuple4ZippedToIterable[A, B, C, D](zipped4: Tuple4Zipped[A, B, C, D]): Iterable[(A, B, C, D)] = new View[(A, B, C, D)] {
    override def iterator(): Iterator[(A, B, C, D)] = zipped4.iterator()
  }
}