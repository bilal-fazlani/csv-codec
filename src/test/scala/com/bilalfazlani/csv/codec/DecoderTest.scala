package com.bilalfazlani.csv.codec

class DecoderTest extends munit.FunSuite {
  case class Case[T: Decoder](
      name: String,
      value: String,
      expected: Either[String, T]
  ) {
    val decode = Decoder[T].decode
  }

  val tests = List(
    Case("Valid Integer", "2", Right(2)),
    Case("String", "lorem", Right("lorem")),
    Case("Empty String", "", Right("")),
    Case("Valid Boolean", "true", Right(true)),
    Case("Valid Option[Boolean]", "true", Right(Option(true))),
    Case("Option[Int]", "3", Right(3)),
    Case[Option[Int]]("No Integer", "", Right(None))
  )
  tests.foreach { t =>
    test(t.name) {
      assertEquals(t.decode(t.value), t.expected)
    }
  }
}
