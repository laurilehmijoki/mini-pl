package frontend

import frontend.Token.{IdentifierToken, IntToken}

case class VerifiedProgram(statements: Seq[StatementSequence])

object SemanticAnalysis {
  def verify(statements: Seq[StatementSequence]): Either[Seq[SemanticError], VerifiedProgram] =
    statements.zipWithIndex.flatMap(Function.tupled { (statement, index) =>
      lazy val statementsBeforeThisStatement = statements.take(index)
      statement match {
        case print: Print =>
          resolveUndeclaredIdentifiers(print, statementsBeforeThisStatement)
        case varDeclaration: VarDeclaration =>
          statementsBeforeThisStatement.collect {
            case another: VarDeclaration if another.identifierToken == varDeclaration.identifierToken =>
              TokenAlreadyDeclared(varDeclaration.identifierToken, another)
          } ++ resolveUndeclaredIdentifiers(varDeclaration, statementsBeforeThisStatement)
      }
    }) match {
      case Nil => Right(VerifiedProgram(statements))
      case errors => Left(errors)
    }

  def findIdentifiers(expression: Expression): Seq[IdentifierToken] =
    expression match {
      case operand: OperandNode =>
        operand.operandToken match {
          case identifierToken: IdentifierToken => identifierToken :: Nil
          case _: IntToken => Nil
        }
      case operator: OperatorNode =>
        findIdentifiers(operator.left) ++ findIdentifiers(operator.right)
    }

  def findIdentifiers(statementSequence: StatementSequence): Seq[IdentifierToken] =
    statementSequence match {
      case print: Print => findIdentifiers(print.expression)
      case varDeclaration: VarDeclaration => findIdentifiers(varDeclaration.expression)
    }

  private def resolveUndeclaredIdentifiers(statementSequence: StatementSequence, statementsBeforeThisStatement: Seq[StatementSequence]) = {
    lazy val referencedIdentifiersInThisStatement = findIdentifiers(statementSequence).toSet
    referencedIdentifiersInThisStatement.flatMap { identifier =>
      val identifierIsDeclared = statementsBeforeThisStatement.exists {
        case varDeclaration: VarDeclaration => referencedIdentifiersInThisStatement.contains(varDeclaration.identifierToken)
        case Print(_) => false
      }
      if (identifierIsDeclared)
        Nil
      else
        IdentifierNotDeclared(identifier) :: Nil
    }
  }

}