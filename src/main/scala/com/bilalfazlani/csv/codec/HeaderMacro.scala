package com.bilalfazlani.csv.codec

import scala.quoted.*

inline def headersOf[T](
    inline namespaced: Boolean = true,
    inline vals: Boolean = true
): Seq[String] = ${
  headerImpl[T]('namespaced, 'vals)
}
inline def csvHeadersOf[T](
    inline namespaced: Boolean = true,
    inline vals: Boolean = true
): String =
  headersOf[T](namespaced, vals).mkString(", ")

def headerImpl[T: Type](using Quotes)(
    namespaced: Expr[Boolean],
    vals: Expr[Boolean]
): Expr[Seq[String]] =
  import quotes.reflect.*
  val tpe = TypeRepr.of[T]
  val result =
    tpe.classSymbol.fold(report.errorAndAbort(s"class not found ${tpe.show}")) {
      t =>
        if t.isDefinedInCurrentRun then
          val fields = Expr(
            getFields(
              tpe.classSymbol.get,
              namespaced.valueOrAbort,
              vals.valueOrAbort,
              None
            )
          )
          fields
        else
          report.errorAndAbort(
            s"This method needs to be used 'inline' or with a compile-time defined Type.\n" +
              s"'${tpe.show}' will not visible at run time"
          )
    }
  // report.info(s"Generated headers: ${result.show}")
  result

private def getFields(using Quotes)(
    symbol: quotes.reflect.Symbol,
    namespaced: Boolean,
    vals: Boolean,
    prefix: Option[String]
): Seq[String] =
  import quotes.reflect.*
  val fields = members(vals, symbol)
  fields.flatMap { f =>
    val innerSymbol = f.termRef.typeSymbol
    val innerFields = members(vals, innerSymbol)
    if innerFields.nonEmpty then
      getFields(innerSymbol, namespaced, vals, Some(f.name))
    else if namespaced then prefix.fold(Seq(f.name))(p => Seq(s"$p.${f.name}"))
    else Seq(f.name)
  }

private def members(using
    Quotes
)(vals: Boolean, symbol: quotes.reflect.Symbol): Seq[quotes.reflect.Symbol] = {
  val valDefs    = symbol.declaredFields.filter(_.isValDef)
  val caseFields = symbol.caseFields
  if vals then valDefs else caseFields
}
