package com.bilalfazlani.csv.codec

import shapeless3.deriving.*

trait Encoder[T]:
  extension (value: T) def encode: String
  private[bilalfazlani] def fieldSize: Int = 1

object Encoder {
  def apply[T: Encoder] = summon[Encoder[T]]

  given product[T](using
      inst: K0.ProductInstances[Encoder, T],
      labelling: Labelling[T]
  ): Encoder[T] = new {
    extension (value: T)
      def encode: String = if (labelling.elemLabels.nonEmpty)
        Range(0, labelling.elemLabels.length)
          .map { i =>
            inst.project(value)(i)(
              [t] => (enc: Encoder[t], x: t) => enc.encode(x)
            )
          }
          .mkString(",")
      else labelling.label

    private[bilalfazlani] override def fieldSize: Int =
      labelling.elemLabels.length
  }

  given coproduct[T](using
      inst: K0.CoproductInstances[Encoder, T],
      labelling: Labelling[T]
  ): Encoder[T] = new {
    extension (value: T)
      def encode: String =
        inst.ordinal(value).asInstanceOf[Encoder[Any]].encode(value)
  }

  inline def derived[T](using gen: K0.Generic[T]): Encoder[T] =
    gen.derive(product, coproduct)
}

given Encoder[String] = a => a

given Encoder[Int] = a => a.toString

given Encoder[Boolean] = a => a.toString

given [T: Encoder]: Encoder[Option[T]] = new {
  private lazy val enc = Encoder[T]
  extension (value: Option[T])
    def encode: String =
      val empty = "," * (enc.fieldSize - 1)
      value.fold(empty)(_.encode)
  private[bilalfazlani] override def fieldSize: Int = enc.fieldSize
}
