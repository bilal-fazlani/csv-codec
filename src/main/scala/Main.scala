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

trait Encoder[T]:
  extension (value: T) def encode: String

given Encoder[String] = new {
  extension (value: String) def encode: String = value
}

given Encoder[Int] = new {
  extension (value: Int) def encode: String = value.toString
}

@main def hello: Unit =
  println(1232.encode)
  println("asds".encode)
  println(Decoder[Int].decode("123"))
  println(Decoder[String].decode("asd12323"))
  println(Decoder[Int].decode("asd12323"))
