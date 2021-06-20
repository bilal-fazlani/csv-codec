package com.bilalfazlani

class ProductEncoderTest extends munit.FunSuite {
  test("Case class") {
    case class Person(name: String, age: Int, employee: Boolean) derives Encoder
    val person = Person("bilal", 20, true)
    assertEquals(person.encode, "bilal,20,true")
  }

  test("Case class with Option") {
    case class Person(name: String, age: Option[Int], employee: Boolean)
        derives Encoder
    val person = Person("bilal", None, true)
    assertEquals(person.encode, "bilal,,true")
  }

  test("Option case class") {
    case class Person(name: String, age: Int, employee: Boolean) derives Encoder
    val person = Option(Person("bilal", 20, true))
    assertEquals(person.encode, "bilal,20,true")
  }

  test("Empty option case class") {
    case class Person(name: String, age: Int, employee: Boolean) derives Encoder
    val person: Option[Person] = None
    assertEquals(person.encode, ",,")
  }
}
