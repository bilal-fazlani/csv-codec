package com.bilalfazlani

import shapeless3.deriving.*
import scala.util.chaining.*

given Show[String] = new {
  extension (t: String) def show = t
}

given Show[Int] = new {
  extension (t: Int) def show = t.toString
}

trait Show[A]:
  extension (t: A) def show: String

object Show:
  inline def derived[A](using gen: K0.Generic[A]): Show[A] =
    gen.derive(product, coproduct)

  given product[A](using
      inst: => K0.ProductInstances[Show, A],
      labelling: Labelling[A]
  ): Show[A] = a =>
    val elems = labelling.elemLabels
    val values = Range(0, labelling.elemLabels.length)
      .map { i =>
        inst.project(a)(i)([t] => (show: Show[t], x: t) => show.show(x))
      }
    elems
      .zip(values)
      .map((a, b) => s"$a = $b")
      .pipe(s =>
        if elems.nonEmpty then s.mkString(labelling.label + "(", ", ", ")")
        else labelling.label
      )

  given coproduct[A](using
      inst: => K0.CoproductInstances[Show, A],
      labelling: Labelling[A]
  ): Show[A] = a =>
    val elems = labelling.elemLabels.toList
    inst.ordinal(a).asInstanceOf[Show[Any]].show(a)

//-------------- TRY OUT -----------------

case class Age(years: Int, months: Int)
case class Person(
    name: String,
    city: String,
    age: Age,
    employeeType: EmployeeType
) derives Show

enum EmployeeType derives Show:
  case Permanent, Contractor

@main def hello11 =
  val p = Person("bilal", "mumbai", Age(20, 12), EmployeeType.Contractor)
  println(p.show)
