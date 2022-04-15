package com.bilalfazlani.csv.codec

class PrimitiveEncodingTest extends munit.FunSuite {
  case class Case[T: Encoder](name: String, value: T, expected: List[String]) {
    val encode = Encoder[T].encode
  }

  val tests = List(
    Case("Integer", 2, List("2")),
    Case("String", "lorem", List("lorem")),
    Case("Empty String", "", List("")),
    Case("Boolean", true, List("true"))
  )
  tests.foreach { t =>
    test(t.name) {
      assertEquals(t.encode(t.value), t.expected)
    }
  }
}
