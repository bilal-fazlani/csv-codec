package com.bilalfazlani.csv.codec

class DecodingPrimitives extends munit.FunSuite {
  case class TestCase[+T: Decoder](
      name: String,
      value: String,
      expected: ParseResult[T]
  ) {
    val decode: String => ParseResult[T] = Decoder[T].decode
  }

  given a[T]: Conversion[T, ParseResult[T]] = x =>
    Right(ParseSuccess(List.empty, x))

  val tests = List(
    TestCase("Valid Integer", "2", 2),
    TestCase("String", "lorem", "lorem"),
    TestCase[String]("Empty String", "", Left(CsvParsingError.NoValue())),
    TestCase("Valid Boolean", "true", true),
    TestCase[Boolean](
      "Invalid Boolean",
      "dd",
      Left(CsvParsingError.InvalidValue("dd", "Boolean"))
    ),
    TestCase[Int](
      "Invalid Int",
      "5f",
      Left(CsvParsingError.InvalidValue("5f", "Int"))
    ),
    TestCase[Int](
      "Remaining value",
      " 5 , a7 , 2b",
      Right(ParseSuccess(List(" a7 ", " 2b"), 5))
    ),
    TestCase[Boolean](
      "Empty String for Boolean",
      "",
      Left(CsvParsingError.NoValue())
    ),
    TestCase[Option[Int]](
      "Option of Int",
      "",
      Right(ParseSuccess(List.empty, None))
    ),
    TestCase[Option[Boolean]](
      "Option of Boolean",
      "",
      Right(ParseSuccess(List.empty, None))
    )
  )
  tests.foreach { t =>
    test(t.name) {
      assertEquals(t.decode(t.value), t.expected)
    }
  }
}
