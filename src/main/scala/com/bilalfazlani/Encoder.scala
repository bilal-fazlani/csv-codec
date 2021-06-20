package com.bilalfazlani

import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

trait Encoder[T]:
  extension (value: T) def encode: String
object Encoder {
  def apply[T: Encoder] = summon[Encoder[T]]
  inline def summonAll[T <: Tuple]: List[Encoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case dd: (t *: ts) => summonInline[Encoder[t]] :: summonAll[ts]

  def sumEncoder[T](
      s: Mirror.SumOf[T],
      encoders: => List[Encoder[_]]
  ): Encoder[T] =
    new Encoder[T]:
      extension (t: T)
        def encode: String =
          val index = s.ordinal(t)
          println(s"s: $s")
          println(s"index: $index")
          val encoder = encoders(index)
          encoder.asInstanceOf[Encoder[Any]].encode(t)

  def productEncoder[T](encoders: => List[Encoder[_]]): Encoder[T] = // (1)
    new Encoder[T]: // (2)
      extension (t: T)
        def encode: String =
          (t.asInstanceOf[Product]
            .productIterator)
            .zip(encoders.iterator)
            .map { case (p, s) =>
              s.asInstanceOf[Encoder[Any]]
                .encode(p)
            }
            .mkString(",")

  inline given derived[T](using m: Mirror.Of[T]): Encoder[T] =
    lazy val encoders = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => sumEncoder(s, encoders)
      case _: Mirror.ProductOf[T] => productEncoder(encoders)
}

given Encoder[String] = new {
  extension (value: String) def encode: String = value
}

given Encoder[Int] = new {
  extension (value: Int) def encode: String = value.toString
}

given Encoder[Boolean] = new {
  extension (value: Boolean) def encode: String = value.toString
}

given [T: Encoder]: Encoder[Option[T]] = new {
  extension (value: Option[T]) def encode: String = value.fold("")(_.encode)
}
