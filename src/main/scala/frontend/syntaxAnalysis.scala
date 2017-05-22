package frontend

import frontend.StatementSequence.StatementParseResult
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
  type StatementParseResult = Either[Seq[ParseError], StatementSequence]
  type StatementParser = (Seq[Token]) => StatementParseResult

  def parse(tokens: Seq[Token]): Seq[Either[ParseError, StatementSequence]] = {
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

    tokenStatements.map { tokensInStatement =>
      def findParser(token: Token): Option[StatementParser] =
        token match {
          case VarKeyword => Some { VarDeclaration.parse }
          case PrintKeyword => Some { Print.parse }
          case _ => None
        }

      val (parser, remainingTokens) = tokensInStatement match {
        case head +: tail =>
          val tailWithoutTerminator = tail.takeWhile {
            case t: StatementTerminator => false
            case _ => true
          }
          (findParser(head), tailWithoutTerminator)
        case _ =>
          (None, Nil)
      }

      val statement: Either[ParseError, StatementSequence] = parser
        .map { parse =>
          parse(remainingTokens) match {
            case Left(errors) => Left(ManyErrors(errors))
            case Right(statementSequence) => Right(statementSequence)
          }
        }
        .getOrElse(Left(SimpleError(s"Cannot find parser for the statement $tokenStatements")))

      statement
    }
  }
}

case class Print(expression: Expression) extends StatementSequence

object Print {
  def parse(tokens: Seq[Token]): StatementParseResult =
    (
      for {
        expressionTokens <- expression.expressionTokens(tokens)
        astRoot <- expression.toAst(expression.toPostfix(expressionTokens))
      } yield Print(astRoot)
    )
      .left.map { _ :: Nil }
}

// "var" <var_ident> ":" <type> [ ":=" <expr> ]
case class VarDeclaration(identifierToken: IdentifierToken, typeKeyword: TypeKeyword, expression: Expression) extends StatementSequence

object VarDeclaration {
  def parse(tokens: Seq[Token]): StatementParseResult =
    tokens match {
      case first +: second +: third +: fourth +: tail =>
        val identifierOrError: Either[ParseError, IdentifierToken] = first match {
          case ident: IdentifierToken => Right(ident)
          case wrongToken => Left(SimpleError(s"Unexpected identifier $wrongToken, expected an identifier"))
        }
        val typePrefixOrError: Either[ParseError, Unit] = second match {
          case TypePrefixToken => Right(Unit)
          case wrongToken => Left(SimpleError(s"Unexpected type prefix $wrongToken, expected $TypePrefixToken"))
        }
        val typeOrError: Either[ParseError, TypeKeyword] = third match {
          case typeKeyword: TypeKeyword => Right(typeKeyword)
          case wrongToken => Left(SimpleError(s"Unexpected type keyword $wrongToken"))
        }

        val assignmentOrError: Either[ParseError, Token] = fourth match {
          case AssignmentToken => Right(AssignmentToken)
          case wrongToken => Left(SimpleError(s"$wrongToken is not the expected $AssignmentToken"))
        }

        val expressionOrError: Either[ParseError, Expression] = for {
          expressionTokens <- expression.expressionTokens(tail)
          astRoot <- expression.toAst(expression.toPostfix(expressionTokens))
        } yield astRoot

        identifierOrError :: typePrefixOrError :: typeOrError :: assignmentOrError :: expressionOrError :: Nil match {
          case Right(identifier: IdentifierToken) +: Right(_) +: Right(typeVal: TypeKeyword) +: Right(_) +: Right(expression: Expression) +: _ =>
            Right(VarDeclaration(identifier, typeVal, expression))
          case xs =>
            Left(xs.collect {
              case Left(parseError) => parseError
            })
        }
      case x => Left(SimpleError(s"Invalid var declaration $x") :: Nil)
    }
}

sealed trait ParseError

case class SimpleError(message: String) extends ParseError
case class ManyErrors(errors: Seq[ParseError]) extends ParseError
case class OperatorAtInvalidPosition(stack: List[Expression], operatorToken: OperatorToken) extends ParseError
case class MalformedStack(stack: List[Expression]) extends ParseError