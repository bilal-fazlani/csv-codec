package com.bilalfazlani.csv.codec

import scala.quoted.*

inline def headersOf[T]: String = ${ headerImpl[T] }

def headerImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  val tpe = TypeRepr.of[T]

  val fields = tpe match
    case tpe: TypeRepr => tpe.classSymbol.get.caseFields

  val names = fields.map(_.name)
  Expr(names.mkString(","))
