package frontend

import frontend.Token._

case class VerifiedProgram(statements: Seq[StatementSequence])

object SemanticAnalysis {
  def verify(statements: Seq[StatementSequence]): Either[Seq[SemanticError], VerifiedProgram] =
    statements.zipWithIndex.flatMap(Function.tupled { (statement, index) =>
      lazy val statementsBeforeThisStatement = statements.take(index)
      statement match {
        case print: Print =>
          resolveUndeclaredIdentifiers(print, statementsBeforeThisStatement) ++
          resolveExpressionErrors(print.expression, statementsBeforeThisStatement)
        case varDeclaration: VarDeclaration =>
          statementsBeforeThisStatement.collect {
            case another: VarDeclaration if another.identifierToken.token == varDeclaration.identifierToken.token =>
              TokenAlreadyDeclared(varDeclaration.identifierToken, another)
          } ++
            resolveExpressionErrors(varDeclaration.expression, statementsBeforeThisStatement) ++
            resolveUndeclaredIdentifiers(varDeclaration, statementsBeforeThisStatement)
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
          case _: IntToken | _ : StringToken => Nil
        }
      case operator: OperatorNode =>
        findIdentifiers(operator.left) ++ findIdentifiers(operator.right)
    }

  def resolveExpressionErrors(expression: Expression, statementsBeforeThisStatement: Seq[StatementSequence]): Seq[InvalidExpression] =
    expression match {
      case operand: OperandNode =>
        Nil
      case operator: OperatorNode =>
        (operator.left, operator.right) match {
          case (leftExpression, rightExpression) =>
            val leftTerminalType = findTerminalType(leftExpression, statementsBeforeThisStatement)
            val rightTerminalType = findTerminalType(rightExpression, statementsBeforeThisStatement)
            val expressionTypesMatch = for {
              leftType <- leftTerminalType
              rightType <- rightTerminalType
            } yield leftType == rightType
            expressionTypesMatch.collect {
              case false => InvalidExpression(expression) :: Nil
            } getOrElse Nil
        }
    }

  def findTerminalType(expression: Expression, statementsBeforeThisStatement: Seq[StatementSequence]): Option[Class[ _ <: Terminal]] =
    expression match {
      case operator: OperatorNode =>
        // TODO is it ok to let the left node determine the expected type?
        findTerminalType(operator.left, statementsBeforeThisStatement.drop(1))
      case operand: OperandNode =>
        operand.operandToken match {
          case s: StringToken => Some(classOf[StringToken])
          case i: IntToken => Some(classOf[IntToken])
          case i: IdentifierToken =>
            statementsBeforeThisStatement
              .reverse // We are interested in the latest definition of the identifier
              .flatMap {
                case varDeclaration: VarDeclaration =>
                  if (varDeclaration.identifierToken.token == i.token)
                    findTerminalType(varDeclaration.expression, statementsBeforeThisStatement.drop(1))
                  else
                    Nil
                case _: Print =>
                  Nil
              }
              .headOption
        }

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