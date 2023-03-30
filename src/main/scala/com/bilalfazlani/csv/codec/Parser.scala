package com.bilalfazlani.csv.codec

case class Success[A](remainder: String, value: A)
type Result[A] = Either[CsvParsingError, Success[A]]

sealed trait Parser[A]

object Parser {
  case class And[A, B](parser1: Parser[A], parser2: Parser[B])
      extends Parser[(A, B)]

  def run[A](input: String)(parser: Parser[A]): Result[A] =
    parser match {
      case And(parser1, parser2) =>
        for {
          success1 <- run(input)(parser1)
          success2 <- run(success1.remainder)(parser2)
        } yield Success(success2.remainder, (success1.value, success2.value))
    }
}
