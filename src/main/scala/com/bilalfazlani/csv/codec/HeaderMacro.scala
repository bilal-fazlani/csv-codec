package com.bilalfazlani.csv.codec

import scala.quoted.*

inline def headersOf[T](inline namespaced: Boolean = true): Seq[String] = ${
  headerImpl[T]('namespaced)
}
inline def csvHeadersOf[T](inline namespaced: Boolean = true): String =
  headersOf[T](namespaced).mkString(", ")

def headerImpl[T: Type](using Quotes)(
    namespaced: Expr[Boolean]
): Expr[Seq[String]] =
  import quotes.reflect.*
  val tpe = TypeRepr.of[T]

  tpe.classSymbol.fold(report.errorAndAbort(s"class not found ${tpe.show}")) {
    t =>
      if t.isDefinedInCurrentRun then
        val fields = Expr(
          getFields(tpe.classSymbol.get, namespaced.valueOrAbort, None)
        )
        fields
      else
        report.errorAndAbort(
          s"This method needs to be used 'inline' or with a compile-time defined Type.\n'${tpe.show}' will not visible at run time"
        )
  }

private def getFields(using Quotes)(
    symbol: quotes.reflect.Symbol,
    namespaced: Boolean,
    prefix: Option[String]
): Seq[String] =
  import quotes.reflect.*
  val fields = symbol.caseFields
  fields.flatMap { f =>
    if f.termRef.typeSymbol.caseFields.nonEmpty then
      getFields(f.termRef.typeSymbol, namespaced, Some(f.name))
    else if namespaced then prefix.fold(Seq(f.name))(p => Seq(s"$p.${f.name}"))
    else Seq(f.name)
  }

