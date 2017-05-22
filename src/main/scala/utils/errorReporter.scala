package utils

import frontend._
import utils.extensions.FormattedString

object errorReporter {

  def createErrorReport(program: String, errors: Seq[ParseError]): String = {
    lazy val indexesToHighlight: Set[Int] = errors.flatMap {
      tokensAssociatedWithError
    }.flatMap { token =>
      val errorRange = token.tokenLocation.startIndex until token.tokenLocation.startIndex + token.token.length
      errorRange
    }.toSet

    lazy val highlightedSourceCode = program.zipWithIndex.flatMap { xs =>
      val chr = xs._1
      val position = xs._2
      if (indexesToHighlight.contains(position))
        chr.toString.red
      else
        chr.toString
    }.mkString

    def headlinesAndDescriptions(parseErrors: Seq[ParseError]): Seq[(String, String)] = parseErrors.flatMap {
      case e: SyntaxError =>
        ("Syntax error", e.message) :: Nil
      case manyErrors: ManyErrors => headlinesAndDescriptions(manyErrors.errors)
      case e: OperatorAtInvalidPosition =>
        ("Operator in invalid position", e.toString) :: Nil
      case e: MalformedStack =>
        ("Malformed stack", e.stack.toString()) :: Nil
    }

    val headlines = headlinesAndDescriptions(errors).map(Function.tupled { (title, message) =>
      s"${title.highlighted}: $message"
    }).mkString("\n")
    
    s"$headlines\n$highlightedSourceCode"
  }

  def tokensAssociatedWithError(e: ParseError): Seq[Token] =
    e match {
      case syntaxError: SyntaxError =>
        syntaxError.tokens
      case OperatorAtInvalidPosition(_, _) | MalformedStack(_) =>
        Nil
      case manyErrors: ManyErrors =>
        manyErrors.errors.flatMap(tokensAssociatedWithError)
    }
}
