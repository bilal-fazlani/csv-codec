package com.bilalfazlani.csv.codec

case class Age(years: Int, months: Int)
case class Person(
    name: String,
    city: String
) derives Encoder

class ADTEncodingTest extends munit.FunSuite {
  test("Case class") {
    val person = Person(
      "bilal",
      "mumbai"
    )
    assertEquals(person.encode, List("bilal", "mumbai"))
  }
}
