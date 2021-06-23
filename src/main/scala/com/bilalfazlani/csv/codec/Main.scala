package com.bilalfazlani.csv.codec

@main def hello: Unit =
  // enum AnimalType derives Encoder:
  // case Pet, Wild

  case class Age(years: Int, months: Int)
  case class Animal(name: String, age: Age) derives Encoder
  val dog = Animal("dog", Age(3, 4))
  println(dog.encode)
