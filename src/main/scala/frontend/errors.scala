package frontend

import frontend.SemanticAnalysis.TerminalType
import frontend.Token.{IdentifierToken, OperatorToken}

sealed trait CompilationError

sealed trait ParseError extends CompilationError

case class SyntaxError(tokens: Seq[Token], message: String) extends ParseError
case class EmptyForLoopBody(tokens: Seq[Token]) extends ParseError
case class ParserNotFound(tokens: Seq[Token]) extends ParseError
case class ManyErrors(errors: Seq[ParseError]) extends ParseError
case class OperatorAtInvalidPosition(stack: List[Expression], operatorToken: OperatorToken) extends ParseError
case class MalformedStack(stack: List[Expression]) extends ParseError

sealed trait SemanticError extends CompilationError
case class IdentifierAlreadyDeclared(conflictingToken: Token, declaredHere: VarDeclaration) extends SemanticError
case class IdentifierNotDeclared(identifierToken: IdentifierToken) extends SemanticError
case class InvalidExpression(expression: Expression) extends SemanticError
case class VarDeclarationWithinForLoop(illegalVarDeclaration: VarDeclaration) extends SemanticError
case class ControlVariableMayNotBeReassigned(illegalReassignment: VarAssignment, controlVariable: IdentifierToken) extends SemanticError
case class IncompatibleTypes(
                              expression: Expression,
                              foundType: TerminalType,
                              expectedType: TerminalType
                            ) extends SemanticError