package com.bilalfazlani.csv.codec

import scala.deriving.Mirror

trait Codec[T] extends Encoder[T], Decoder[T]

object Codec {
  def apply[T: Codec] = summon[Codec[T]]

  inline def derived[T](using Mirror.Of[T]): Codec[T] =
    val encoder = Encoder.derived[T]
    val decoder = Decoder.derived[T]
    new {
      export encoder.*
      export decoder.*
    }
}
