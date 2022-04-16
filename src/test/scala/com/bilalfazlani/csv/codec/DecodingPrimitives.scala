package com.bilalfazlani.csv.codec

class DecodingPrimitives extends munit.FunSuite {
  case class TestCase[T: Decoder](
      name: String,
      value: String,
      expected: Either[CsvParsingError, T]
  ) {
    val decode = Decoder[T].decode
  }

  val tests = List(
    TestCase("Valid Integer", "2", Right(2)),
    TestCase("String", "lorem", Right("lorem")),
    TestCase("Empty String", "", Right("")),
    TestCase("Valid Boolean", "true", Right(true)),
    TestCase[Boolean](
      "Invalid Boolean",
      "dd",
      Left(CsvParsingError.InvalidValue("dd", "Boolean"))
    ),
    TestCase[Boolean](
      "Invalid Int",
      "5f",
      Left(CsvParsingError.InvalidValue("5f", "Int"))
    )
  )
  tests.foreach { t =>
    test(t.name) {
      assertEquals(t.decode(t.value), t.expected)
    }
  }
}
