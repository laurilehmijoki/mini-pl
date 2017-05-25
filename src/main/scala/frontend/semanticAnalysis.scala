package frontend

import frontend.Token._

case class VerifiedProgram(statements: Seq[StatementSequence])

object SemanticAnalysis {
  def verify(statements: Seq[StatementSequence]): Either[Seq[CompilationError], VerifiedProgram] =
    statements.zipWithIndex.flatMap(Function.tupled { (statement, index) =>
      lazy val statementsBeforeThisStatement = statements.take(index)
      statement match {
        case print: Print =>
          implicit val expectedReturnType: ExpectedReturnType = None
          resolveUndeclaredIdentifiers(print, statementsBeforeThisStatement) ++
          resolveExpressionErrors(print.expression, statementsBeforeThisStatement)
        case varAssignment: VarAssignment =>
          implicit val expectedReturnType: ExpectedReturnType = None // TODO repetition here, can we remove it?
          resolveUndeclaredIdentifiers(varAssignment, statementsBeforeThisStatement) ++
            resolveExpressionErrors(varAssignment.expression, statementsBeforeThisStatement)
        case varDeclaration: VarDeclaration =>
          implicit val expectedReturnType: ExpectedReturnType = Some(varDeclaration.typeKeyword match {
            case _: StringTypeKeyword => classOf[StringToken]
            case _: IntTypeKeyword => classOf[IntToken]
          })
          statementsBeforeThisStatement.collect {
            case another: VarDeclaration if another.identifierToken.token == varDeclaration.identifierToken.token =>
              IdentifierAlreadyDeclared(varDeclaration.identifierToken, another)
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

  type TerminalType = Class[_ <: Terminal]
  type ExpectedReturnType = Option[TerminalType]

  def resolveExpressionErrors(expression: Expression, statementsBeforeThisStatement: Seq[StatementSequence])(implicit expectedResultType: ExpectedReturnType): Seq[CompilationError] = {
    val errorOrTerminalType: Either[InvalidExpression, TerminalType] = expression match {
      case operand: OperandNode =>
        findTerminalType(operand, statementsBeforeThisStatement)
          .map(Right(_))
          .getOrElse(Left(InvalidExpression(expression)))
      case operator: OperatorNode =>
        (operator.left, operator.right) match {
          case (leftExpression, rightExpression) =>
            val leftTerminalType: Option[TerminalType] = findTerminalType(leftExpression, statementsBeforeThisStatement)
            val rightTerminalType: Option[TerminalType] = findTerminalType(rightExpression, statementsBeforeThisStatement)
            val sharedTerminalType: Option[TerminalType] = for {
              leftType <- leftTerminalType
              rightType <- rightTerminalType
              if leftType == rightType
            } yield leftType

            sharedTerminalType.map(Right(_)).getOrElse(Left(InvalidExpression(expression)))
        }
    }

    def resultTypeNotExpected(terminalType: TerminalType): Option[IncompatibleTypes] = for {
      expectedTerminalType <- expectedResultType
      error <-
      if (terminalType == expectedTerminalType)
        None
      else
        Some(IncompatibleTypes(expression, terminalType, expectedTerminalType))
    } yield error

    errorOrTerminalType.fold(
      error => error :: Nil,
      terminalType => resultTypeNotExpected(terminalType).map(_ :: Nil).getOrElse(Nil)
    )
  }

  def findTerminalType(expression: Expression, statementsBeforeThisStatement: Seq[StatementSequence]): Option[TerminalType] =
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
                    varDeclaration.typeKeyword match {
                      case _: IntTypeKeyword => classOf[IntToken] :: Nil
                      case _: StringTypeKeyword => classOf[StringToken] :: Nil
                    }
                  else
                    Nil
                case _: Print | _: VarAssignment =>
                  Nil
              }
              .headOption
        }
    }

  def referencedIdentifiers(statementSequence: StatementSequence): Seq[IdentifierToken] =
    statementSequence match {
      case print: Print => findIdentifiers(print.expression)
      case varAssignment: VarAssignment => varAssignment.identifierToken +: findIdentifiers(varAssignment.expression)
      case varDeclaration: VarDeclaration => findIdentifiers(varDeclaration.expression)
    }

  private def resolveUndeclaredIdentifiers(statementSequence: StatementSequence, statementsBeforeThisStatement: Seq[StatementSequence]) = {
    val referencedIdentifiersInThisStatement = referencedIdentifiers(statementSequence).toSet
    referencedIdentifiersInThisStatement.flatMap { identifier =>
      val identifierIsDeclared = statementsBeforeThisStatement.exists {
        case varDeclaration: VarDeclaration => referencedIdentifiersInThisStatement.contains(varDeclaration.identifierToken)
        case Print(_) | VarAssignment(_, _) => false
      }
      if (identifierIsDeclared)
        Nil
      else
        IdentifierNotDeclared(identifier) :: Nil
    }
  }

}