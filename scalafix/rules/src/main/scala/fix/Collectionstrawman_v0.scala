package fix

import scalafix._
import scalafix.syntax._
import scalafix.util._
import scala.meta._

case class Collectionstrawman_v0(index: SemanticdbIndex)
  extends SemanticRule(index, "Collectionstrawman_v0") {

  implicit class XtensionSymbolCollection(symbol: Symbol) {
    def name: String = symbol match {
      case Symbol.Global(_, sig) => sig.name
      case _                     => symbol.syntax
    }
  }

  def replaceExtensionMethods(ctx: RuleCtx): Patch = {
    val unimports = Map(
      Symbol("_root_.scala.Predef.augmentString.") ->
        Symbol("_root_.strawman.collection.stringToStringOps."),
      Symbol("_root_.scala.Predef.intArrayOps.") ->
        Symbol("_root_.strawman.collection.arrayToArrayOps.")
    )
    val additionalUnimports = Map(
      "augmentString" -> "wrapString",
      "wrapString" -> "augmentString",
      "intArrayOps" -> "genericArrayOps"
    )
    val visited = collection.mutable.Set.empty[String]
    val toImport = for {
      r <- ctx.index.names
      in = r.symbol.normalized
      out <- unimports.get(in).toList
      if !visited(out.name)
    } yield {
      visited += out.name
      val name = in.name
      def visiting(s: String): List[String] =
        if (!visited(s)) {
          visited += s
          s :: Nil
        } else Nil
      val names = visiting(name) ::: additionalUnimports.get(name)
        .fold(List.empty[String])(visiting)
      if (names.isEmpty) ctx.addGlobalImport(out)
      else {
        ctx.addGlobalImport(out) +
          ctx.addGlobalImport(
            Importer(q"scala.Predef", names.map(n => Importee.Unimport(Name(n)))))
      }
    }
    val predefUnderscore =
      if (toImport.isEmpty) Patch.empty
      else ctx.addGlobalImport(importer"scala.Predef._")
    toImport.asPatch + predefUnderscore
  }

  def removeGlobalImports(ctx: RuleCtx): Patch =
    ctx.removeGlobalImport(Symbol("_root_.scala.collection.")) +
      ctx.removeGlobalImport(Symbol("_root_.scala.collection.concurrent.")) +
      ctx.removeGlobalImport(Symbol("_root_.scala.collection.immutable.")) +
      ctx.removeGlobalImport(Symbol("_root_.scala.collection.mutable."))

  def replaceRange(ctx: RuleCtx): Patch = {
    val rangeImport = Symbol("_root_.strawman.collection.immutable.Range.")
    val inclusiveRange = Symbol(
      "_root_.scala.runtime.RichInt#to(I)Lscala/collection/immutable/Range/Inclusive;.")
    val rangeSymbol = Symbol(
      "_root_.scala.runtime.RichInt#until(I)Lscala/collection/immutable/Range;.")
    ctx.tree.collect {
      case tree @ Term.ApplyInfix(lhs, op, _, arg :: Nil)
          if op.symbol.contains(inclusiveRange) =>
        ctx.replaceTree(tree, q"Range.inclusive($lhs, $arg)".syntax) +
          ctx.addGlobalImport(rangeImport)
      case tree @ Term.ApplyInfix(lhs, op, _, arg :: Nil)
          if op.symbol.contains(rangeSymbol) =>
        ctx.replaceTree(tree, q"Range($lhs, $arg)".syntax) +
          ctx.addGlobalImport(rangeImport)
    }.asPatch
  }

  def replaceSymbols(ctx: RuleCtx): Patch = {
    def c(name: String) =
      s"scala.collection.$name" -> s"strawman.collection.$name"
    def p(name: String) =
      s"scala.Predef.$name" -> s"strawman.collection.immutable.$name"
    def s(name: String, rename: Option[String] = None) =
      s"scala.$name" -> s"strawman.collection.immutable.${rename.getOrElse(name)}"
    def i(name: String, rename: Option[String] = None) =
      s"scala.collection.immutable.$name" ->
        s"strawman.collection.immutable.${rename.getOrElse(name)}"
    def m(name: String) =
      s"scala.collection.mutable.$name" -> s"strawman.collection.mutable.$name"
    ctx.replaceSymbols(
      "scala.Seq" -> "strawman.collection.Seq",
      c("Seq"),
      i("Seq"),
      m("Seq"),
      "scala.IndexedSeq" -> "strawman.collection.IndexedSeq",
      c("IndexedSeq"),
      i("IndexedSeq"),
      c("LinearSeq"),
      i("LinearSeq"),
      p("Set"),
      c("Set"),
      i("Set"),
      m("Set"),
      c("BitSet"),
      i("BitSet"),
      m("BitSet"),
      i("HashSet"),
      m("HashSet"),
      m("LinkedHashSet"),
      c("SortedSet"),
      i("SortedSet"),
      m("SortedSet"),
      i("TreeSet"),
      m("TreeSet"),
      c("Map"),
      i("Map"),
      m("Map"),
      p("Map"),
      i("HashMap"),
      m("HashMap"),
      m("LinkedHashMap"),
      "scala.collection.concurrent.Map" -> "strawman.collection.concurrent.Map",
      "scala.collection.concurrent.TrieMap" -> "strawman.collection.concurrent.TrieMap",
      c("SortedMap"),
      i("SortedMap"),
      m("SortedMap"),
      i("TreeMap"),
      m("TreeMap"),
      s("List"),
      i("List"),
      s("Nil"),
      i("Nil"),
      s("`::`"),
      i("`::`"),
      s("`+:`"),
      i("`+:`"),
      s("`:+`"),
      i("`:+`"),
      s("Stream", Some("LazyList")),
      i("Stream", Some("LazyList")),
      "scala.`#::`" -> "strawman.collection.immutable.LazyList.`#::`",
      m("ListBuffer"),
      i("ListMap"),
      i("ListSet"),
      "scala.Traversable" -> "strawman.collection.Iterable",
      "scala.collection.Traversable" -> "strawman.collection.Iterable",
      "scala.collection.immutable.Traversable" -> "strawman.collection.immutable.Iterable",
      "scala.collection.mutable.Traversable" -> "strawman.collection.mutable.Iterable",
      "scala.Iterable" -> "strawman.collection.Iterable",
      c("Iterable"),
      i("Iterable"),
      m("Iterable"),
      "scala.Iterator" -> "strawman.collection.Iterator",
      c("Iterator"),
      "scala.BufferedIterator" -> "strawman.collection.BufferedIterator",
      "scala.StringBuilder" -> "strawman.collection.mutable.StringBuilder",
      m("StringBuilder"),
      c("BufferedIterator"),
      s("Vector"),
      i("Vector"),
      m("ArrayBuffer")
    )
  }

  def replaceToList(ctx: RuleCtx): Patch = {
    val toTpe = SymbolMatcher.normalized(
      Symbol("_root_.scala.collection.TraversableLike.to.")
    )
    val iterator = SymbolMatcher.normalized(
      Symbol("_root_.scala.collection.LinearSeqLike.iterator."),
      Symbol("_root_.scala.collection.TraversableLike.toIterator.")
    )
    ctx.tree.collect {
      case iterator(t: Name) =>
        ctx.replaceTree(t, "iterator()")
      case toTpe(n: Name) =>
        (for {
          name <- n.tokens.lastOption
          open <- ctx.tokenList.find(name)(t => t.is[Token.LeftBracket])
          close <- ctx.matchingParens.close(open.asInstanceOf[Token.LeftBracket])
        } yield
          ctx.replaceToken(open, "(") +
            ctx.replaceToken(close, ")")
          ).asPatch
    }.asPatch
  }

  def replaceTupleZipped(ctx: RuleCtx): Patch = {
    val tupleZipped = SymbolMatcher.normalized(
      Symbol("_root_.scala.runtime.Tuple2Zipped.Ops.zipped."),
      Symbol("_root_.scala.runtime.Tuple3Zipped.Ops.zipped.")
    )
    ctx.tree.collect {
      case tupleZipped(Term.Select(Term.Tuple(args), name)) =>
        val removeTokensPatch =
          (for {
            zipped <- name.tokens.headOption
            closeTuple <- ctx.tokenList.leading(zipped).find(_.is[Token.RightParen])
            openTuple <- ctx.matchingParens.open(closeTuple.asInstanceOf[Token.RightParen])
            maybeDot = ctx.tokenList.slice(closeTuple, zipped).find(_.is[Token.Dot])
          } yield {
            ctx.removeToken(openTuple) +
              maybeDot.map(ctx.removeToken).asPatch +
              ctx.removeToken(zipped)
          }).asPatch

        def removeSurroundingWhiteSpaces(tk: Token) =
          (ctx.tokenList.trailing(tk).takeWhile(_.is[Token.Space]).map(ctx.removeToken) ++
            ctx.tokenList.leading(tk).takeWhile(_.is[Token.Space]).map(ctx.removeToken)).asPatch

        val commas =
          for {
            (prev, next) <- args.zip(args.tail)
            tokensBetweenArgs = ctx.tokenList.slice(prev.tokens.last, next.tokens.head)
            comma <- tokensBetweenArgs.find(_.is[Token.Comma])
          } yield comma

        val replaceCommasPatch = commas match {
          case head :: tail =>
            ctx.replaceToken(head, ".lazyZip(") +
              removeSurroundingWhiteSpaces(head) ++
              tail.map { comma =>
                ctx.replaceToken(comma, ").lazyZip(") +
                  removeSurroundingWhiteSpaces(comma)
              }
          case _ => Patch.empty
        }

        removeTokensPatch + replaceCommasPatch
    }.asPatch
  }

  def replaceJavaConverters(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case Import(List(importer)) if importer.syntax.containsSlice("collection.JavaConverters._") =>
        ctx.replaceTree(importer, "strawman.collection.JavaConverters._")
    }.asPatch
  }

  override def fix(ctx: RuleCtx): Patch = {
    replaceToList(ctx) +
      replaceSymbols(ctx) +
      replaceExtensionMethods(ctx) +
      replaceRange(ctx) +
      replaceTupleZipped(ctx) +
      replaceJavaConverters(ctx) +
      removeGlobalImports(ctx)
  }
}
