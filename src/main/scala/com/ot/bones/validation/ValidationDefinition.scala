package com.ot.bones.validation

import java.net.URI
import java.util.UUID

import cats.free.FreeApplicative
import cats.implicits._
import com.ot.bones.interpreter.ExtractionInterpreter.ValidationResultNel

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}



object ValidationDefinition {
  /** Represents a validation operation */
  trait ValidationOp[T] {
    def isValid: T => Boolean
    def defaultError(t: T): String
    def lift: Validation[T] = FreeApplicative.lift(this)
    def description: String
  }

  type Validation[A] = FreeApplicative[ValidationOp, A]


  case class ValidValue[T](validValues: Vector[T]) extends ValidationOp[T] {
    override def isValid: (T) => Boolean = validValues.contains
    override def defaultError(t: T): String = s"Value ${t} must be one of ${validValues.mkString("('","','","')")}"
    override def description: String = s"one of ${validValues.mkString("('","','","')")}"
  }

  case class InvalidValue[T](invalidValues: Vector[T]) extends ValidationOp[T] {
    override def isValid: (T) => Boolean = str => {! invalidValues.contains(str)}
    override def defaultError(t: T): String = s"Value ${t} must not be one of ${invalidValues.mkString("('","','","')")}"
    override def description: String = s"not one of ${invalidValues.mkString("('","','","')")}"
  }

  trait BaseValidationOp[T] {
    def validValue(validValues: Vector[T]) = ValidValue(validValues)
    def invalidValue(invalidValues: Vector[T]) = InvalidValue(invalidValues)
  }

  private val alphanumRegx = "^[a-zA-Z0-9]*$".r

  object StringValidation extends BaseValidationOp[String] {

    case class IsAlphanum() extends ValidationOp[String] {
      val isValid: String => Boolean = alphanumRegx.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not alphanumeric"

      override def description: String = s"alphanumeric"
    }

    case class Min(min: Int) extends ValidationOp[String] {
      val isValid: String => Boolean = _.length >= min

      override def defaultError(t: String): String = s"$t is less than $min"

      override def description: String = s"minimum of $min"
    }

    case class Max(max: Int) extends ValidationOp[String] {
      val isValid: String => Boolean = _.length <= max

      override def defaultError(t: String): String = s"$t is greater than $max"

      override def description: String = s"maximum of $max"
    }

    case class MatchesRegex(r: Regex) extends ValidationOp[String] {
      val isValid: String => Boolean = r.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t does not match regular expression ${r.toString}"

      override def description: String = s"must match regular expression ${r.toString}"
    }

    case class Length(length: Int) extends ValidationOp[String] {
      override def isValid: (String) => Boolean = _.length == length

      override def defaultError(t: String): String = s"$t does not have length $length"

      override def description: String = s"length of $length"
    }

    case class Custom(f: String => Boolean, defaultErrorF: String => String, description: String) extends ValidationOp[String] {
      val isValid: String => Boolean = f

      override def defaultError(t: String): String = defaultErrorF(t)
    }

    case class Guid() extends ValidationOp[String] {
      val isValid: String => Boolean = str => Try {
        UUID.fromString(str)
      } match {
        case Success(_) => true
        case Failure(_) => false
      }

      override def defaultError(t: String): String = s"$t is not a GUID"

      override def description: String = "be a GUID"
    }

    case class Uppercase() extends ValidationOp[String] {
      val isValid: String => Boolean = str => str.toUpperCase === str

      override def defaultError(t: String): String = s"$t must be uppercase"

      override def description: String = "uppercase"
    }

    case class CreditCard() extends ValidationOp[String] {
      override def isValid: (String) => Boolean = input => ValidationUtil.luhnCheck(10, input)

      override def defaultError(t: String): String = s"$t is not a valid credit card number"

      override def description: String = "valid credit card number"
    }

    private val tokenRegex = "^[a-zA-Z0-9_]*$".r

    case class Token() extends ValidationOp[String] {
      override def isValid: (String) => Boolean = tokenRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a token"

      override def description: String = "token"
    }

    // From https://stackoverflow.com/questions/13912597/validate-email-one-liner-in-scala
    val emailRegex: Regex =
      """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

    case class Email() extends ValidationOp[String] {
      override def isValid: (String) => Boolean = emailRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a valid email"

      override def description: String = "email"
    }

    val hexRegex: Regex = "^[0-9A-F]+$".r

    case class Hex() extends ValidationOp[String] {
      override def isValid: (String) => Boolean = hexRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not hexadecimal"

      override def description: String = "hexadecimal"
    }

    val base64Regex: Regex = "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$".r

    case class Base64() extends ValidationOp[String] {
      override def isValid: (String) => Boolean = input => base64Regex.findFirstMatchIn(input.trim).isDefined

      override def defaultError(t: String): String = s"$t is not Base64"

      override def description: String = "Base64"
    }

    val hostnameRegex: Regex = "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])$".r

    case class Hostname() extends ValidationOp[String] {
      override def isValid: (String) => Boolean = hostnameRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a hostname"

      override def description: String = "RFC1123 hostname"
    }

    val ipv4Regex: Regex = "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$".r

    case class Ipv4() extends ValidationOp[String] {
      override def isValid: String => Boolean = ipv4Regex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not an ipv4"

      override def description: String = "IPv4"
    }

    case class Lowercase() extends ValidationOp[String] {
      override def isValid: (String) => Boolean = str => str.toLowerCase === str

      override def defaultError(t: String): String = s"$t is not all lowercase"

      override def description: String = "lowercase"
    }

    case class Uri() extends ValidationOp[String] {
      override def isValid: String => Boolean = input => Try {
        URI.create(input)
      }.isSuccess

      override def defaultError(t: String): String = s"$t is not a Uri"

      override def description: String = "URI"
    }

    type ValidateString[A] = String => ValidationResultNel[A]


    /** Length of string must be equal to theLength param */
    def length(theLength: Int): ValidationOp[String] = Length(theLength)

    /** Length must be greater than minLength param */
    def min(minLength: Int): ValidationOp[String] = Min(minLength)

    /** Length must be less than maxLength param */
    def max(maxLength: Int): ValidationOp[String] = Max(maxLength)

    /** String must match specified Regex */
    def matchesRegex(r: Regex): ValidationOp[String] = MatchesRegex(r)

    /** String must be alpha numeric */
    def alphanum(): ValidationOp[String] = IsAlphanum()

    /** String must be a guid */
    def guid(): ValidationOp[String] = Guid()

    /** String must be a valid email format */
    def email(): ValidationOp[String] = Email()

    /** String must be a token, which is alpha numeric with underscore. */
    def token(): ValidationOp[String] = Token()

    /** String must be a valid hexadecimal String */
    def hex(): ValidationOp[String] = Hex()

    /** String must be in base64 */
    def base64(): ValidationOp[String] = Base64()

    /** String must be a hostname */
    def hostname(): ValidationOp[String] = Hostname()

    /** String must be an IPv4 */
    def iPv4(): ValidationOp[String] = Ipv4()

    /** String must be all lowercase, that is all letters in the string must be lowercase. */
    def lowercase(): ValidationOp[String] = Lowercase()

    /** String must be a Uri */
    def uri(): ValidationOp[String] = Uri()
  }


  object IntValidation extends BaseValidationOp[Int] {

    case class Between(min: Int, max: Int) extends ValidationOp[Int] {
      val isValid: Int => Boolean = input => input >= min && input <= max

      override def defaultError(t: Int): String = s"$t is not between $min and $max"

      override def description: String = s"between $min and $max"

    }

    case class Max(maxInt: Int) extends ValidationOp[Int] {
      override def isValid: Int => Boolean = _ <= maxInt

      override def defaultError(t: Int): String = s"$t is greater than $maxInt"

      override def description: String = s"maximum of $maxInt"
    }

    case class Min(minInt: Int) extends ValidationOp[Int] {
      override def isValid: Int => Boolean = _ >= minInt

      override def defaultError(t: Int): String = s"$t is less than $minInt"

      override def description: String = s"minimum of $minInt"
    }

    case class Greater(greaterThan: Int) extends ValidationOp[Int] {
      override def isValid: Int => Boolean = _ > greaterThan

      override def defaultError(t: Int): String = s"$t is not greater than $greaterThan"

      override def description: String = s"greater than $greaterThan"
    }

    case class Less(lessThan: Int) extends ValidationOp[Int] {
      override def isValid: Int => Boolean = _ < lessThan

      override def defaultError(t: Int): String = s"$t is not less than $lessThan"

      override def description: String = s"less than $lessThan"
    }

    case class Multiple(multipleOf: Int) extends ValidationOp[Int] {
      override def isValid: Int => Boolean = _ % multipleOf === 0

      override def defaultError(t: Int): String = s"$t is not a multiple of $multipleOf"

      override def description: String = s"multiple of $multipleOf"
    }

    case class Positive() extends ValidationOp[Int] {
      override def isValid: Int => Boolean = _ >= 0

      override def defaultError(t: Int): String = s"$t is not positive"

      override def description: String = s"positive"
    }

    case class Negative() extends ValidationOp[Int] {
      override def isValid: Int => Boolean = _ <= 0

      override def defaultError(t: Int): String = s"$t is not negative"

      override def description: String = s"negative"
    }


    def between(min: Int, max: Int): ValidationOp[Int] = Between(min, max)

    def max(maxValue: Int): ValidationOp[Int] = Max(maxValue)

    def min(minValue: Int): ValidationOp[Int] = Min(minValue)

    def greater(value: Int): ValidationOp[Int] = Greater(value)

    def less(value: Int): ValidationOp[Int] = Less(value)
  }

}