package utils

sealed trait FormattingContext

object BashShell extends FormattingContext
object Markdown extends FormattingContext

object extensions {
  implicit class FormattedString(val original: String) extends AnyVal {
    def highlighted(implicit formattingContext: FormattingContext): String =
      formattingContext match {
        case BashShell => s"\033[1m$original\033[0m"
        case Markdown => original
      }

    def error(implicit formattingContext: FormattingContext): String =
      formattingContext match {
        case BashShell => s"\033[31m$original\033[0m"
        case Markdown => original
      }
  }
}