package com.bilalfazlani

import shapeless3.deriving.*

trait ShapelessEncoder[T]:
  extension (value: T) def encode: String

object ShapelessEncoder {
  def apply[T: ShapelessEncoder] = summon[ShapelessEncoder[T]]

  given product[T](using
      inst: K0.ProductInstances[ShapelessEncoder, T],
      labelling: Labelling[T]
  ): ShapelessEncoder[T] = a =>
    if (labelling.elemLabels.nonEmpty)
      Range(0, labelling.elemLabels.length)
        .map { i =>
          inst.project(a)(i)(
            [t] => (enc: ShapelessEncoder[t], x: t) => enc.encode(x)
          )
        }
        .mkString(",")
    else labelling.label

  given coproduct[T](using
      inst: K0.CoproductInstances[ShapelessEncoder, T],
      labelling: Labelling[T]
  ): ShapelessEncoder[T] = a =>
    inst.ordinal(a).asInstanceOf[ShapelessEncoder[Any]].encode(a)

  inline def derived[T](using gen: K0.Generic[T]): ShapelessEncoder[T] =
    gen.derive(product, coproduct)
}

given ShapelessEncoder[String] = a => a

given ShapelessEncoder[Int] = a => a.toString

given ShapelessEncoder[Boolean] = a => a.toString

given [T: ShapelessEncoder]: ShapelessEncoder[Option[T]] = a =>
  a.fold("")(_.encode)

