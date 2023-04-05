package com.bilalfazlani.csv.codec

trait HeaderEncoder[A]:
  def encodeHeaders: Seq[String]
  def encodeHeadersString: String = encodeHeaders.mkString(",")

object HeaderEncoder:
  def apply[A](using HeaderEncoder[A]): HeaderEncoder[A] =
    summon[HeaderEncoder[A]]

  inline def derived[A]: HeaderEncoder[A] = gen[A]

  inline given gen[A]: HeaderEncoder[A] = new HeaderEncoder[A]:
    def encodeHeaders: Seq[String] = headersOf[A]()
