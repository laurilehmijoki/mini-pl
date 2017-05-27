package frontend

import frontend.StatementSequence.{ParseResult, errorOrExpression}
import frontend.Token._

sealed trait Program

/*
 <stmt>   ::=  "var" <var_ident> ":" <type> [ ":=" <expr> ]
           |   <var_ident> ":=" <expr>
           |   "for" <var_ident> "in" <expr> ".." <expr> "do"
                  <stmts> "end" "for"
           |   "read" <var_ident>
           |   "print" <expr>
           |   "assert" "(" <expr> ")"
 */
sealed trait StatementSequence extends Program

object StatementSequence {
  type ParseResult = Either[ParseError, StatementSequence]
  type StatementParser = (Seq[Token]) => Option[ParseResult]

  def parse(tokens: Seq[Token]): Seq[ParseResult] = {
    val tokenStatements = tokens.foldLeft((None: Option[Token], Seq[Seq[Token]]())) { (memo, token) =>
      val previousToken = memo._1
      val tokenStatements = memo._2
      val (currentStatement, nextStatements) = previousToken match {
        case Some(_: StatementTerminator) | None =>
          (Nil, tokenStatements)
        case _ =>
          (
            tokenStatements.lastOption.getOrElse(Nil),
            tokenStatements.take(tokenStatements.length - 1)
          )
      }
      (
        Some(token),
        nextStatements :+ (currentStatement :+ token)
      )
    }._2

    val parsers =
      { VarDeclaration.parse(_) } ::
      { VarAssignment.parse(_) } ::
      { Print.parse(_) } ::
        Nil

    tokenStatements.map { tokensInStatement =>
      parsers.foldLeft(None: Option[ParseResult]) { (result, parse) =>
        result match {
          case parsedResult@Some(_) => parsedResult
          case None => parse(tokensInStatement)
        }
      } getOrElse Left(ParserNotFound(tokensInStatement))
    }
  }

  def errorOrExpression(tokens: Seq[Token]): Either[ParseError, Expression] = for {
    expressionTokens <- expr.expressionTokens(tokens)
    astRoot <- expr.toExpression(expr.toPostfix(expressionTokens))
  } yield astRoot
}
// <var_ident> ":=" <expr>
case class VarAssignment(identifierToken: IdentifierToken, expression: Expression) extends StatementSequence

object VarAssignment {
  def parse(tokens: Seq[Token]): Option[ParseResult] =
    tokens match {
      case (identifierToken: IdentifierToken) +: (_: AssignmentToken) +: third :+ (_: SemicolonToken) =>
        Some(errorOrExpression(third).map(expression => VarAssignment(identifierToken, expression)))
      case _ =>
        None
    }
}

// "print" <expr>
case class Print(expression: Expression) extends StatementSequence

object Print {
  def parse(tokens: Seq[Token]): Option[ParseResult] =
    tokens match {
      case (_: PrintKeyword) +: second :+ (_: SemicolonToken) =>
        Some(errorOrExpression(second).map(Print(_)))
      case _ =>
        None
    }
}

// "var" <var_ident> ":" <type> [ ":=" <expr> ]
case class VarDeclaration(identifierToken: IdentifierToken, typeKeyword: TypeKeyword, expression: Option[Expression]) extends StatementSequence

object VarDeclaration {
  def parse(tokens: Seq[Token]): Option[ParseResult] =
    tokens match {
      case (_: VarKeyword) +: second +: third +: fourth +: ((_: SemicolonToken) :: Nil) => // // "var" <var_ident> ":" <type>
        val errorOrVarStatement = identifierOrError(second, tokens) :: typePrefixOrError(third, tokens) :: typeOrError(fourth,tokens) :: Nil match {
          case Right(identifier: IdentifierToken) +: Right(_) +: Right(typeVal: TypeKeyword) +: Nil =>
            Right(VarDeclaration(identifier, typeVal, None))
          case xs =>
            Left(ManyErrors(xs.collect {
              case Left(parseError) => parseError
            }))
        }
        Some(errorOrVarStatement)
      case (_: VarKeyword) +: second +: third +: fourth +: fifth +: tail :+ (_: SemicolonToken) => // "var" <var_ident> ":" <type> ":=" <expr>
        val assignmentOrError: Either[ParseError, Token] = fifth match {
          case assignment: AssignmentToken => Right(assignment)
          case wrongToken => Left(SyntaxError(tokens, s"$wrongToken is not the expected $AssignmentToken"))
        }

        val errorOrVarStatement = identifierOrError(second, tokens) :: typePrefixOrError(third, tokens) :: typeOrError(fourth,tokens) :: assignmentOrError :: errorOrExpression(tail) :: Nil match {
          case Right(identifier: IdentifierToken) +: Right(_) +: Right(typeVal: TypeKeyword) +: Right(_) +: Right(expression: Expression) +: _ =>
            Right(VarDeclaration(identifier, typeVal, Some(expression)))
          case xs =>
            Left(ManyErrors(xs.collect {
              case Left(parseError) => parseError
            }))
        }
        Some(errorOrVarStatement)
      case _ =>
        None
    }

  def identifierOrError(token: Token, tokens: Seq[Token]): Either[ParseError, IdentifierToken] = token match {
    case ident: IdentifierToken => Right(ident)
    case wrongToken => Left(SyntaxError(tokens, s"Unexpected identifier $wrongToken, expected an identifier"))
  }

  def typePrefixOrError(token: Token, tokens: Seq[Token]): Either[ParseError, Unit] = token match {
    case TypePrefixToken(_) => Right(Unit)
    case wrongToken => Left(SyntaxError(tokens, s"Unexpected type prefix $wrongToken, expected $TypePrefixToken"))
  }

  def typeOrError(token: Token, tokens: Seq[Token]): Either[ParseError, TypeKeyword] = token match {
    case typeKeyword: TypeKeyword => Right(typeKeyword)
    case wrongToken => Left(SyntaxError(tokens, s"Unexpected type keyword $wrongToken"))
  }
}
