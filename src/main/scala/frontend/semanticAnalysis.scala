package frontend

import frontend.Token.{IdentifierToken, IntToken}

case class VerifiedProgram(statements: Seq[StatementSequence])

object SemanticAnalysis {
  def verify(statements: Seq[StatementSequence]): Either[Seq[SemanticError], VerifiedProgram] =
    statements.zipWithIndex.flatMap(Function.tupled { (statement, index) =>
      lazy val statementsBeforeThisStatement = statements.take(index)
      statement match {
        case print: Print =>
          lazy val identifiersInPrintExpression = findIdentifiers(print.expression).toSet
          identifiersInPrintExpression.flatMap { identifier =>
            val identifierIsDeclared = statementsBeforeThisStatement.exists {
              case varDeclaration: VarDeclaration if identifiersInPrintExpression.contains(varDeclaration.identifierToken) => true
            }
            if (identifierIsDeclared)
              Nil
            else
              IdentifierNotDeclared(identifier) :: Nil
          }
        case varDeclaration: VarDeclaration =>
          statementsBeforeThisStatement.collect {
            case another: VarDeclaration if another.identifierToken == varDeclaration.identifierToken =>
              TokenAlreadyDeclared(varDeclaration.identifierToken, another)
          }
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

}