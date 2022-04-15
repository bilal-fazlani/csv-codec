package com.bilalfazlani.csv.codec

class DecoderTest extends munit.FunSuite {

  test("Invalid Integer") {
    val value = "2s".parse[Int]
    assertEquals(value, Left(CsvParsingError.InvalidValue("2s", "Int")))
  }

  test("Invalid Boolean") {
    val value = "ss".parse[Boolean]
    assertEquals(value, Left(CsvParsingError.InvalidValue("ss", "Boolean")))
  }

  case class Case[T: Decoder](
      name: String,
      value: String,
      expected: Either[CsvParsingError, T]
  ) {
    val decode = Decoder[T].decode
  }

  val tests = List(
    Case("Valid Integer", "2", Right(2)),
    Case("String", "lorem", Right("lorem")),
    Case("Empty String", "", Right("")),
    Case("Valid Boolean", "true", Right(true))
  )
  tests.foreach { t =>
    test(t.name) {
      assertEquals(t.decode(t.value), t.expected)
    }
  }
}
