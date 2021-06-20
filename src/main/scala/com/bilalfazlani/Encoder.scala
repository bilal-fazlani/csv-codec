package com.bilalfazlani

trait Encoder[T]:
  extension (value: T) def encode: String
object Encoder {
  def apply[T: Encoder] = summon[Encoder[T]]
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
