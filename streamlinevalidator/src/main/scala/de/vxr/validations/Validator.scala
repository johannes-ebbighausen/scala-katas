package de.vxr.validations

import de.vxr.Line

import scala.util.matching.Regex

sealed trait Result

final case object Ok extends Result

final case class Error(row: Int, msg: String) extends Result


trait Validator {
  def validate(line: Line, row: Int): Result
}

case class RegExValidator(regEx: Regex, column: Int) extends Validator {
  override def validate(line: Line, row: Int): Result = if (regEx.matches(line.cols(column)))
    Ok
  else Error(row, s"mismatches regular expression in column $column")
}

case class NotEmptyValidator(column: Int) extends Validator {
  override def validate(line: Line, row: Int): Result = if (line.cols(column).trim.isEmpty)
    Error(row, s"has empty column $column")
  else Ok
}