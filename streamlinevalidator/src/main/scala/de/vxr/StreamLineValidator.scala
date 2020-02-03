package de.vxr

import de.vxr.validations.{Error, NotEmptyValidator, Ok, RegExValidator, Result, Validator}

/**
 *  Problems and todos
 *
 *  - if last col(s) is empty, there could be an index out of bounds exception, because gdrive does not provides \t's for this col(s) => index out of bounds happens
 *  - general tsv-syntax check (lines, cols)
 *  - error-reporting beautify and correctify ;-)
 *  - line be parameterised with delimter, so one could use csv's as well
 *  - testfile raus -> scala test rein ;-)
 *  - stream error's out
 */

case class Line(in: String) {
  val cols = in.split("\t")
}


object StreamLineValidator extends App {

  val validations: Seq[Validator] = Seq(
    RegExValidator("(bird|mammal)_(\\d|a|b|c|d|e|f)+".r, 0),
    NotEmptyValidator(1))

  val bufferedSource = io.Source.fromFile("testfile.tsv")

  private val result: Seq[Result] = bufferedSource
    .getLines
    .zipWithIndex
    .foldLeft(Seq.empty[Result])((result, line) => {
      val (str, row) = line
      result ++ validations.map(_.validate(Line(str), row))
    })

  val (oks, errors: Seq[Error]) = result.partition(_ == Ok)

  println(
    s"""
       |lines total: ${result.length}
       |ok: ${oks.length}
       |errors: ${errors.length}
       |report: ${errors.map(e => s"row ${e.row} ${e.msg}").mkString(", ")}
       |""".stripMargin)

}
