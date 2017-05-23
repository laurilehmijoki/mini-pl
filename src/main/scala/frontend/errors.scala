package frontend

import frontend.Token.{IdentifierToken, OperatorToken}

sealed trait CompilationError

sealed trait ParseError extends CompilationError

case class SyntaxError(tokens: Seq[Token], message: String) extends ParseError
case class ParserNotFound(tokens: Seq[Token]) extends ParseError
case class ManyErrors(errors: Seq[ParseError]) extends ParseError
case class OperatorAtInvalidPosition(stack: List[Expression], operatorToken: OperatorToken) extends ParseError
case class MalformedStack(stack: List[Expression]) extends ParseError

sealed trait SemanticError extends CompilationError
case class TokenAlreadyDeclared(conflictingToken: Token, declaredHere: VarDeclaration) extends SemanticError
case class IdentifierNotDeclared(identifierToken: IdentifierToken) extends SemanticError
case class InvalidExpression(expression: Expression) extends SemanticError