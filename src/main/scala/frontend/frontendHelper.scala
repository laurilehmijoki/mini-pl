package frontend

object frontendHelper {

  type VerificationResult = Either[Seq[CompilationError], VerifiedProgram]

  def verify(program: String): VerificationResult = {
    val tokens = Token.tokenize(program)

    val errorsOrStatements = StatementSequence.parse(tokens).foldLeft(Right(Nil): Either[Seq[ParseError], Seq[StatementSequence]]) {
      (memo, item) => item match {
        case Left(errors) =>
          val previousErrors = memo.left.getOrElse(Nil)
          Left(previousErrors :+ errors)
        case Right(statementSequences) =>
          memo.right.map { _ :+ statementSequences}
      }
    }

    errorsOrStatements.right.flatMap(SemanticAnalysis.verify)
  }
}
