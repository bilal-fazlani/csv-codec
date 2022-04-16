package com.bilalfazlani.csv.codec

import magnolia1.*

enum CsvParsingError:
  case InvalidValue(value: String, targetType: String)
  case InsufficientValues

extension (str: String) {
  def parse[T: Decoder] = summon[Decoder[T]].decode(str)
}

case class ParseSuccess[+A](remainder: List[String], value: A)
type ParseResult[T] = Either[CsvParsingError, ParseSuccess[T]]

def fail[A](error: CsvParsingError): Either[CsvParsingError, A] = Left(error)
def success[A](value: A, remainder: List[String]) = Right(
  ParseSuccess(remainder, value)
)

trait Decoder[T]:
  def decode(str: String): ParseResult[T]

object Decoder extends AutoDerivation[Decoder] {
  def apply[T: Decoder] = summon[Decoder[T]]

  def join[T](ctx: CaseClass[Decoder.Typeclass, T]): Decoder[T] = value =>
    val parseResult = ctx.params
      .foldLeft[Option[ParseResult[Seq[Any]]]](None) { (state, p) =>
        state match {
          //first param
          case None =>
            Some(
              p.typeclass
                .decode(value)
                .map(x => ParseSuccess(x.remainder, Seq(x.value)))
            )
          //nothing left
          case Some(Right(previous)) if previous.remainder.isEmpty =>
            Some(Left(CsvParsingError.InsufficientValues))
          //next params
          case Some(Right(previous)) =>
            Some(
              p.typeclass
                .decode(previous.remainder.mkString(","))
                .map(x =>
                  (ParseSuccess(x.remainder, previous.value.appended(x.value)))
                )
            )
          case Some(l @ Left(_)) =>
            Some(l)
        }
      }
      .fold(fail(CsvParsingError.InsufficientValues))(identity)
    //returnEither[CsvParsingError, ParseSuccess[T]]
    parseResult.map(x => ParseSuccess(x.remainder, ctx.rawConstruct(x.value)))

  def split[T](ctx: SealedTrait[Decoder, T]): Decoder[T] = value => ???

  given Decoder[String] = s =>
    s.split(",").toList match {
      case Nil          => fail(CsvParsingError.InsufficientValues)
      case head :: tail => success(head.trim, tail)
    }
  given Decoder[Int] = s =>
    s.split(",").toList match {
      case Nil => fail(CsvParsingError.InsufficientValues)
      case head :: tail =>
        head.trim.toIntOption
          .fold(fail(CsvParsingError.InvalidValue(head, "Int")))(int =>
            success(int, tail)
          )
    }
  given Decoder[Boolean] = s =>
    s.split(",").toList match {
      case Nil => fail(CsvParsingError.InsufficientValues)
      case head :: tail =>
        head.trim.toBooleanOption
          .fold(fail(CsvParsingError.InvalidValue(head, "Boolean")))(bool =>
            success(bool, tail)
          )
    }
}
