package com.bilalfazlani.csv.codec

case class Employee(name: String, age: Int, permanent: Boolean) derives Decoder

class ProductDecoderTest extends munit.FunSuite {
  test("Valid CSV") {
    val person = "bilal,20,true"
    assertEquals(person.parse[Employee], Right(Employee("bilal", 20, true)))
  }

  test("Insufficient params") {
    val str = "bilal"
    assertEquals(
      str.parse[Employee],
      Left(CsvParsingError.InsufficientValues)
    )
  }

  test("Invalid param type") {
    val person = "bilal,1k3,false"
    assertEquals(
      person.parse[Employee],
      Left(CsvParsingError.InvalidValue("1k3", "Int"))
    )
  }
}
