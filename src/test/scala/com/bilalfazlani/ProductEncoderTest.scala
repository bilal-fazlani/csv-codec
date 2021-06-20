package com.bilalfazlani

case class Person(name: String, age: Int, employee: Boolean)

given Encoder[Person] = new {
  extension (value: Person)
    def encode: String =
      List(
        value.name.encode,
        value.age.encode,
        value.employee.encode
      ).mkString(",")
}

class ProductEncoderTest extends munit.FunSuite {
  test("Case class") {
    val person = Person("bilal", 20, true)
    assertEquals(person.encode, "bilal,20,true")
  }

  test("Option case class") {
    val person = Option(Person("bilal", 20, true))
    assertEquals(person.encode, "bilal,20,true")
  }

  test("Empty option case class") {
    val person: Option[Person] = None
    assertEquals(person.encode, ",,")
  }
}
