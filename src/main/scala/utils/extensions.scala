package utils

sealed trait FormattingContext

object BashShell extends FormattingContext
object Html extends FormattingContext

object extensions {
  implicit class FormattedString(val original: String) extends AnyVal {
    def highlighted(implicit formattingContext: FormattingContext): String =
      formattingContext match {
        case BashShell => s"\033[1m$original\033[0m"
        case Html => s"""<span style="font-weight: bold;">$original</span>"""
      }

    def red(implicit formattingContext: FormattingContext): String =
      formattingContext match {
        case BashShell => s"\033[31m$original\033[0m"
        case Html => s"""<span style="color: #ff0033;">$original</span>"""
      }

    def green(implicit formattingContext: FormattingContext): String =
      formattingContext match {
        case BashShell => s"\033[32m$original\033[0m"
        case Html => s"""<span style="color: #42d647;">$original</span>"""
      }
  }
}