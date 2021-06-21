package com.bilalfazlani

import shapeless3.deriving.*

trait ShapelessEncoder[T]:
  extension (value: T) def encode: String
  private[bilalfazlani] def fieldSize: Int = 1

object ShapelessEncoder {
  def apply[T: ShapelessEncoder] = summon[ShapelessEncoder[T]]

  given product[T](using
      inst: K0.ProductInstances[ShapelessEncoder, T],
      labelling: Labelling[T]
  ): ShapelessEncoder[T] = new {
    extension (value: T)
      def encode: String = if (labelling.elemLabels.nonEmpty)
        Range(0, labelling.elemLabels.length)
          .map { i =>
            inst.project(value)(i)(
              [t] => (enc: ShapelessEncoder[t], x: t) => enc.encode(x)
            )
          }
          .mkString(",")
      else labelling.label

    private[bilalfazlani] override def fieldSize: Int =
      labelling.elemLabels.length
  }

  given coproduct[T](using
      inst: K0.CoproductInstances[ShapelessEncoder, T],
      labelling: Labelling[T]
  ): ShapelessEncoder[T] = new {
    extension (value: T)
      def encode: String =
        inst.ordinal(value).asInstanceOf[ShapelessEncoder[Any]].encode(value)
  }

  inline def derived[T](using gen: K0.Generic[T]): ShapelessEncoder[T] =
    gen.derive(product, coproduct)
}

given ShapelessEncoder[String] = a => a

given ShapelessEncoder[Int] = a => a.toString

given ShapelessEncoder[Boolean] = a => a.toString

given [T: ShapelessEncoder]: ShapelessEncoder[Option[T]] = new {
  private lazy val enc = ShapelessEncoder[T]
  extension (value: Option[T])
    def encode: String =
      value.fold(List.fill(enc.fieldSize)("").mkString(","))(_.encode)
  private[bilalfazlani] override def fieldSize: Int = enc.fieldSize
}

