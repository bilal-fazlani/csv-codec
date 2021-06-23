package com.bilalfazlani.csv.codec

case class Age(years: Int, months: Int)
case class Person(
    name: String,
    bla: Option[Option[Option[Int]]],
    middleName: Option[String],
    city: String,
    age: Age,
    parentAge: Option[Age],
    employeeType: EmployeeType,
    department: Option[Department]
) derives Encoder

enum EmployeeType derives Encoder:
  case Permanent, Contractor

enum Department derives Encoder:
  case Marketing
  case Sales
  case Custom(departmentId: Int, departmentName: String)

class ADTEncodingTest extends munit.FunSuite {
  test("Case class") {
    val person = Person(
      "bilal",
      Some(Some(Some(3))),
      None,
      "mumbai",
      Age(20, 12),
      None,
      EmployeeType.Contractor,
      None
    )
    assertEquals(person.encode, "bilal,3,,mumbai,20,12,,,Contractor,")
  }

  test("Option case class") {
    val person = Option(
      Person(
        "bilal",
        None,
        None,
        "mumbai",
        Age(20, 12),
        None,
        EmployeeType.Contractor,
        None
      )
    )
    assertEquals(person.encode, "bilal,,,mumbai,20,12,,,Contractor,")
  }

  test("Empty option case class") {
    val person: Option[Person] = None
    assertEquals(person.encode, ",,,,,,,,,")
  }
}
