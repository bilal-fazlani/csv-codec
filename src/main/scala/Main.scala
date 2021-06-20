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

given Decoder[Person] = new {
  def decode(str: String): Either[String, Person] =
    str.split(",").toList match {
      case nameStr :: ageStr :: empStr :: Nil =>
        for {
          age <- Decoder[Int].decode(ageStr)
          emp <- Decoder[Boolean].decode(empStr)
        } yield (Person(nameStr, age, emp))
      case _ => Left(s"[$str could not be parsed into Person]")
    }
}

@main def hello: Unit =
  println(1232.encode)
  println("asds".encode)
  println(Decoder[Int].decode("123"))
  println(Decoder[String].decode("asd12323"))
  println(Decoder[Int].decode("asd12323"))

  val person = Person("bilal", 20, true).encode
  println(person.encode)
  println(Decoder[Person].decode("bilal,20,true"))
