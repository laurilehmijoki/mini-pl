package frontend

import frontend.Statement.ParseResult

object frontendHelper {

  type VerificationResult = Either[Seq[CompilationError], VerifiedProgram]

  def verify(program: String): VerificationResult = {
    val tokens = Token.tokenize(program)

    errorsOrStatements(Statement.parse(tokens)).right.flatMap(SemanticAnalysis.verify)
  }

  def errorsOrStatements(parseResult: Seq[ParseResult]) =
    parseResult.foldLeft(Right(Nil): Either[Seq[ParseError], Seq[Statement]]) {
      (memo, item) =>
        item match {
          case Left(errors) =>
            val previousErrors = memo.left.getOrElse(Nil)
            Left(previousErrors :+ errors)
          case Right(statements) =>
            memo.right.map {
              _ :+ statements
            }
        }
    }
}
