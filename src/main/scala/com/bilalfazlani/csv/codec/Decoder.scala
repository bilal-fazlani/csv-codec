package com.bilalfazlani.csv.codec

import magnolia1.*

enum CsvParsingError:
  case InvalidValue(value: String, targetType: String)
  case InsufficientValues

extension (str: String) {
  def parse[T: Decoder] = summon[Decoder[T]].decode(str)
}

type ParseResult[T] = Either[CsvParsingError, T]

trait Decoder[T]:
  def decode(str: String): ParseResult[T]

object Decoder extends AutoDerivation[Decoder] {
  def apply[T: Decoder] = summon[Decoder[T]]

  def join[T](ctx: CaseClass[Decoder.Typeclass, T]): Decoder[T] = value =>
    ctx
      .constructEither { p =>
        val strings     = parse(value)
        if (p.index < strings.length) then p.typeclass.decode(strings(p.index))
        else Left(CsvParsingError.InsufficientValues)
      }
      .left
      .map(_.head)

  def split[T](ctx: SealedTrait[Decoder, T]): Decoder[T] = value => ???

  private def parse(str: String): List[String] =
    str.split(",").map(_.trim).toList

  given Decoder[String] = s => Right(s)
  given Decoder[Int] = s =>
    s.toIntOption.toRight(CsvParsingError.InvalidValue(s, "Int"))
  given Decoder[Boolean] = s =>
    s.toBooleanOption.toRight(CsvParsingError.InvalidValue(s, "Boolean"))
}
