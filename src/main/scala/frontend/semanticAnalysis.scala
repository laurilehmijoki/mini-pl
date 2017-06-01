package frontend

import frontend.Token._

case class VerifiedProgram(statements: Seq[Statement])

object SemanticAnalysis {
  def verify(statements: Seq[Statement]): Either[Seq[CompilationError], VerifiedProgram] =
    statements.zipWithIndex.flatMap(Function.tupled { (statement, index) =>
      lazy val statementsBeforeThisStatement = statements.take(index)
      statement match {
        case print: Print =>
          resolveUndeclaredIdentifiers(print, statementsBeforeThisStatement) ++
          resolveExpressionErrors(print.expression, statementsBeforeThisStatement, expectedResultType = None)
        case varAssignment: VarAssignment =>
          resolveUndeclaredIdentifiers(varAssignment, statementsBeforeThisStatement) ++
            resolveExpressionErrors(varAssignment.expression, statementsBeforeThisStatement, expectedResultType = None)
        case forLoop: ForLoop =>
          resolveUndeclaredIdentifiers(forLoop, statementsBeforeThisStatement) ++
            resolveExpressionErrors(forLoop.from, statementsBeforeThisStatement, expectedResultType = Some(classOf[IntToken])) ++
            resolveExpressionErrors(forLoop.to, statementsBeforeThisStatement, expectedResultType = Some(classOf[IntToken]))

            // TODO verify that the control variable is not reassigned within the loop

            // TODO Also verify that there are no VarDeclarations in the loop
        case varDeclaration: VarDeclaration =>
          val expectedReturnType: ExpectedReturnType = Some(varDeclaration.typeKeyword match {
            case _: StringTypeKeyword => classOf[StringToken]
            case _: IntTypeKeyword => classOf[IntToken]
          })
          statementsBeforeThisStatement.collect {
            case another: VarDeclaration if another.identifierToken.token == varDeclaration.identifierToken.token =>
              IdentifierAlreadyDeclared(varDeclaration.identifierToken, another)
          } ++
            varDeclaration.expression.map(expression => resolveExpressionErrors(expression, statementsBeforeThisStatement, expectedReturnType)).getOrElse(Nil) ++
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

  def resolveExpressionErrors(expression: Expression, statementsBeforeThisStatement: Seq[Statement], expectedResultType: ExpectedReturnType): Seq[CompilationError] = {
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

  def findTerminalType(expression: Expression, statementsBeforeThisStatement: Seq[Statement]): Option[TerminalType] =
    expression match {
      case operator: OperatorNode =>
        // TODO is it ok to let the left node determine the expected type?
        findTerminalType(operator.left, statementsBeforeThisStatement.drop(1))
      case operand: OperandNode =>
        operand.operandToken match {
          case _: StringToken => Some(classOf[StringToken])
          case _: IntToken => Some(classOf[IntToken])
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
                case _: Print | _: VarAssignment | _: ForLoop =>
                  Nil
              }
              .headOption
        }
    }

  def referencedIdentifiers(statement: Statement): Seq[IdentifierToken] =
    statement match {
      case print: Print => findIdentifiers(print.expression)
      case varAssignment: VarAssignment => varAssignment.identifierToken +: findIdentifiers(varAssignment.expression)
      case forLoop: ForLoop => (forLoop.identifierToken +: findIdentifiers(forLoop.from)) ++ findIdentifiers(forLoop.to)
      case varDeclaration: VarDeclaration => varDeclaration.expression.map(findIdentifiers).getOrElse(Nil)
    }

  private def resolveUndeclaredIdentifiers(statement: Statement, statementsBeforeThisStatement: Seq[Statement]) = {
    val referencedIdentifiersInThisStatement = referencedIdentifiers(statement).toSet
    referencedIdentifiersInThisStatement.flatMap { identifier =>
      val identifierIsDeclared = statementsBeforeThisStatement.exists {
        case varDeclaration: VarDeclaration => referencedIdentifiersInThisStatement.contains(varDeclaration.identifierToken)
        case _: Print | _: VarAssignment | _: ForLoop => false
      }
      if (identifierIsDeclared)
        Nil
      else
        IdentifierNotDeclared(identifier) :: Nil
    }
  }

}