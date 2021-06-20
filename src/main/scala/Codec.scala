import scala.util.Try
//------------------------------------ ENCODING -------------------------------

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

//------------------------------------ ENCODING -------------------------------

trait Encoder[T]:
  extension (value: T) def encode: String

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
