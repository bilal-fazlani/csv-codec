package com.bilalfazlani.csv.codec

import shapeless3.deriving.*

trait Codec[T] extends Encoder[T], Decoder[T]

object Codec {
  def apply[T: Codec] = summon[Codec[T]]

  inline def derived[T](using gen: K0.Generic[T]): Codec[T] =
    val encoder = Encoder.derived[T]
    val decoder = Decoder.derived[T]
    new {
      export encoder.*
      export decoder.*
    }
}
