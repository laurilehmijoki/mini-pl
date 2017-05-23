package utils

import frontend._
import utils.extensions.FormattedString

object errorReporter {

  def createErrorReport(program: String, errors: Seq[CompilationError])(implicit formattingContext: FormattingContext): String = {
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
        chr.toString.error
      else
        chr.toString
    }.mkString

    def headlinesAndDescriptions(parseErrors: Seq[CompilationError]): Seq[(String, String)] = parseErrors.flatMap {
      case e: IncompatibleTypes =>
        ("Type error", s"Expected ${e.expectedType.getSimpleName} but got ${e.foundType.getSimpleName} in expression ${e.expression}") :: Nil
      case e: IdentifierAlreadyDeclared =>
        ("Syntax error", "Token is already declared") :: Nil
      case e: IdentifierNotDeclared =>
        ("Syntax error", s"""Identifier "${e.identifierToken.token}" is not declared""") :: Nil
      case e: ParserNotFound =>
        ("Syntax error", s"Cannot find parser for tokens ${e.tokens}") :: Nil
      case e: SyntaxError =>
        ("Syntax error", e.message) :: Nil
      case manyErrors: ManyErrors => headlinesAndDescriptions(manyErrors.errors)
      case e: OperatorAtInvalidPosition =>
        ("Operator in invalid position", e.toString) :: Nil
      case e: MalformedStack =>
        ("Malformed stack", e.stack.toString()) :: Nil
      case e: InvalidExpression =>
        ("Invalid expression", e.expression.toString) :: Nil
    }

    val headlines = headlinesAndDescriptions(errors).map(Function.tupled { (title, message) =>
      s"${title.highlighted}: $message"
    }).mkString("\n")
    
    s"$headlines\n$highlightedSourceCode"
  }

  def tokensAssociatedWithError(e: CompilationError): Seq[Token] =
    e match {
      case e: IncompatibleTypes => tokens(e.expression)
      case e: IdentifierAlreadyDeclared =>
        e.conflictingToken :: Nil
      case e: IdentifierNotDeclared =>
        e.identifierToken :: Nil
      case e: ParserNotFound =>
        e.tokens
      case syntaxError: SyntaxError =>
        syntaxError.tokens
      case e: InvalidExpression =>
        def findInvalidToken(expression: Expression): Token =
          expression match {
            case operand: OperandNode => operand.operandToken
            case operator: OperatorNode => findInvalidToken(operator.right)
          }
        findInvalidToken(e.expression) :: Nil
      case OperatorAtInvalidPosition(_, _) | MalformedStack(_) =>
        Nil
      case manyErrors: ManyErrors =>
        manyErrors.errors.flatMap(tokensAssociatedWithError)
    }

  def tokens(expression: Expression): Seq[Token] =
    expression match {
      case operand: OperandNode => operand.operandToken :: Nil
      case operator: OperatorNode => tokens(operator.left) ++ Seq(operator.operatorToken) ++ tokens(operator.right)
    }
}
