package utils

object extensions {
  implicit class FormattedString(val original: String) extends AnyVal {
    def highlighted: String = s"\033[1m$original\033[0m"
    def red: String = s"\033[31m$original\033[0m"
  }
}