package com.bilalfazlani

import scala.util.Try
import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

extension (str: String) def parse[A: Decoder] = Decoder[A].decode(str)

trait Decoder[T]:
  def decode(str: String): Either[String, T]
object Decoder {
  def apply[T: Decoder] = summon[Decoder[T]]

  // inline def summonAll[T <: Tuple]: List[Decoder[_]] =
  //   inline erasedValue[T] match
  //     case _: EmptyTuple => Nil
  //     case _: (t *: ts)  => summonInline[Decoder[t]] :: summonAll[ts]

  // def sumDecoder[T](
  //     s: Mirror.SumOf[T],
  //     decoders: => List[Decoder[_]]
  // ): Decoder[T] =
  //   new Decoder[T]:
  //     def decode(str: String): Either[String, T] =
  //       ???

  // def productDecoder[T](decoders: => List[Decoder[_]]): Decoder[T] = // (1)
  //   new Decoder[T]: // (2)
  //     def decode(str: String): Either[String, T] =
  //       ???

  // inline given derived[T](using m: Mirror.Of[T]): Decoder[T] =
  //   lazy val decoders = summonAll[m.MirroredElemTypes]
  //   inline m match
  //     case s: Mirror.SumOf[T]     => sumDecoder(s, decoders)
  //     case _: Mirror.ProductOf[T] => productDecoder(decoders)
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
