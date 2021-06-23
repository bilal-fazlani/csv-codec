package com.bilalfazlani.csv.codec

class PrimitiveEncodingTest extends munit.FunSuite {
  case class Case[T: Encoder](name: String, value: T, expected: String) {
    val encode = Encoder[T].encode
  }

  val tests = List(
    Case("Integer", 2, "2"),
    Case("String", "lorem", "lorem"),
    Case("Empty String", "", ""),
    Case("Boolean", true, "true"),
    Case("Option[Boolean]", Option(true), "true"),
    Case("Option[Int]", Option(3), "3"),
    Case[Option[Int]]("None (Int)", None, "")
  )
  tests.foreach { t =>
    test(t.name) {
      assertEquals(t.encode(t.value), t.expected)
    }
  }
}
