package frontend

import frontend.StatementSequence.ParseResult

object frontendHelper {

  type VerificationResult = Either[Seq[CompilationError], VerifiedProgram]

  def verify(program: String): VerificationResult = {
    val tokens = Token.tokenize(program)

    errorsOrStatements(StatementSequence.parse(tokens)).right.flatMap(SemanticAnalysis.verify)
  }

  def errorsOrStatements(parseResult: Seq[ParseResult]) =
    parseResult.foldLeft(Right(Nil): Either[Seq[ParseError], Seq[StatementSequence]]) {
      (memo, item) =>
        item match {
          case Left(errors) =>
            val previousErrors = memo.left.getOrElse(Nil)
            Left(previousErrors :+ errors)
          case Right(statementSequences) =>
            memo.right.map {
              _ :+ statementSequences
            }
        }
    }
}
