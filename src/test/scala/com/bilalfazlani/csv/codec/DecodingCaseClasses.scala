package com.bilalfazlani.csv.codec

case class Employee(name: String, age: Int, permanent: Boolean) derives Decoder

class DecodingCaseClasses extends munit.FunSuite {
  test("Valid CSV") {
    val person = "bilal,20,true"
    assertEquals(
      person.parse[Employee],
      Right(ParseSuccess(List.empty, Employee("bilal", 20, true)))
    )
  }

  test("Nested valid CSV") {
    case class Brand(brandName: String, country: String)
    case class Model(brand: Brand, modelName: String)
    case class Vehicle(number: String, model: Model) derives Decoder

    val vehicalString = "MH0123, HYUNDAI, KOREA, CRETA"
    val vehical = Vehicle("MH0123", Model(Brand("HYUNDAI", "KOREA"), "CRETA"))
    assertEquals(
      vehicalString.parse[Vehicle],
      Right(ParseSuccess(List.empty, vehical))
    )
  }

  test("With Optional primitive field") {
    case class Brand(brandName: String, country: Option[String])
    case class Model(brand: Brand, modelName: String)
    case class Vehicle(number: String, model: Model) derives Decoder

    val vehicalString = "MH0123, HYUNDAI, , CRETA"
    val vehical = Vehicle("MH0123", Model(Brand("HYUNDAI", None), "CRETA"))
    assertEquals(
      vehicalString.parse[Vehicle],
      Right(ParseSuccess(List.empty, vehical))
    )
  }

  test("With Optional case class field".only) {
    case class Brand(brandName: String, country: Option[String])
    case class Model(brand: Option[Brand], modelName: String)
    case class Vehicle(number: String, model: Model) derives Decoder

    val vehicalString = "MH0123, , , CRETA"
    val vehical       = Vehicle("MH0123", Model(None, "CRETA"))
    assertEquals(
      vehicalString.parse[Vehicle],
      Right(ParseSuccess(List.empty, vehical))
    )
  }

  test("Insufficient params") {
    val str = "bilal"
    assertEquals(
      str.parse[Employee],
      Left(CsvParsingError.NoValue("age"))
    )
  }

  test("Invalid param type") {
    val person = "bilal,1k3,false"
    assertEquals(
      person.parse[Employee],
      Left(CsvParsingError.InvalidValue("1k3", "Int", "age"))
    )
  }
}
