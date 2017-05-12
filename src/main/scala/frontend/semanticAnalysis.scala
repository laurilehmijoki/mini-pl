package frontend

sealed trait SemanticError
case class TokenAlreadyDeclared(declaredHere: VarDeclaration) extends SemanticError

object SemanticAnalysis {
  def verify(statements: Seq[StatementSequence]): Seq[SemanticError] =
    statements.zipWithIndex.flatMap(Function.tupled { (statement, index) =>
      statement match {
        case print: Print => Nil // TODO check that referenced identifier is declared
        case varDeclaration: VarDeclaration =>
          statements.take(index).collect {
            case another: VarDeclaration if another.identifierToken == varDeclaration.identifierToken =>
              TokenAlreadyDeclared(another)
          }
      }
    })
}