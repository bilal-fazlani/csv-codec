package com.bilalfazlani.csv.codec

@main def hello: Unit =
  // enum AnimalType derives Encoder:
  // case Pet, Wild

  case class Age(years: Int, months: Int)
  case class Animal(name: String, age: Age) derives Codec

  val aa = Animal("asdasd", Age(2, 4))
  println("sdasd".parse[Animal])
  println(aa.encode)
