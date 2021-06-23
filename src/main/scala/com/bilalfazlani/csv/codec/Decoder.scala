package com.bilalfazlani.csv.codec

import scala.util.Try
import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

extension (str: String) def parse[A: Decoder] = Decoder[A].decode(str)

trait Decoder[T]:
  def decode(str: String): Either[String, T]
object Decoder {
  def apply[T: Decoder] = summon[Decoder[T]]
}

given Decoder[String] = new {
  def decode(str: String): Either[String, String] = Right(str)
}

given Decoder[Int] = new {
  def decode(str: String): Either[String, Int] =
    str.toIntOption.toRight(s"$str is not a valid Int")
}

given Decoder[Boolean] = new {
  def decode(str: String): Either[String, Boolean] =
    str.toBooleanOption.toRight(s"$str is not a valid Boolean")
}

given [T: Decoder]: Decoder[Option[T]] = new {
  def decode(str: String): Either[String, Option[T]] =
    str.trim match {
      case "" => Right(None)
      case x  => Decoder[T].decode(str).map(Some.apply)
    }
}
