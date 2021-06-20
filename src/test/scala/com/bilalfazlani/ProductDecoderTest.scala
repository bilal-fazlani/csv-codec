package com.bilalfazlani

case class Employee(name: String, age: Int, permanent: Boolean)

given Decoder[Employee] = new {
  def decode(str: String): Either[String, Employee] =
    str.split(",").toList match {
      case nameStr :: ageStr :: empStr :: Nil =>
        for {
          age       <- Decoder[Int].decode(ageStr)
          permanent <- Decoder[Boolean].decode(empStr)
        } yield (Employee(nameStr, age, permanent))
      case _ => Left(s"[$str] could not be parsed into Employee")
    }
}

class ProductDecoderTest extends munit.FunSuite {
  test("Valid CSV") {
    val person = "bilal,20,true"
    assertEquals(person.parse[Employee], Right(Employee("bilal", 20, true)))
  }

  test("Insufficient params") {
    val person = "bilal"
    assertEquals(
      person.parse[Employee],
      Left("[bilal] could not be parsed into Employee")
    )
  }

  test("Invalid param type") {
    val person = "bilal,1k3,false"
    assertEquals(
      person.parse[Employee],
      Left("1k3 is not a valid Int")
    )
  }
}
