package com.bilalfazlani.csv.codec

import magnolia1.*

trait Encoder[T]:
  extension (value: T) def encode: List[String]

object Encoder extends AutoDerivation[Encoder] {
  def apply[T: Encoder] = summon[Encoder[T]]

  def join[T](ctx: CaseClass[Encoder.Typeclass, T]): Encoder[T] = value =>
    ctx.params.foldLeft[List[String]](List.empty) { (acc, p) =>
      acc ++ p.typeclass.encode(p.deref(value))
    }

  def split[T](ctx: SealedTrait[Encoder, T]): Encoder[T] = value =>
    ctx.choose(value) { sub =>
      sub.typeclass.encode(sub.cast(value))
    }

  given Encoder[String]  = s => List(s)
  given Encoder[Int]     = s => List(s.toString)
  given Encoder[Boolean] = s => List(s.toString)
}
