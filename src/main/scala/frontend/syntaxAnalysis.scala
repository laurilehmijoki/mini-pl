package frontend

import frontend.StatementSequence.{ParseResult, errorOrExpression}
import frontend.Token._
import frontend.VarDeclaration._

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
  type Parser = (Seq[Token]) => Option[(ParseResult, Seq[Token])]

  val parsers: Seq[Parser] =
    { VarDeclaration.parse(_) } ::
      { VarAssignment.parse(_) } ::
      { Print.parse(_) } ::
      { ForLoop.parse(_) } ::
      Nil

  def parse(tokens: Seq[Token]): Seq[ParseResult] = {
    var parserFound = false
    parsers flatMap { parseStatement =>
      if (!parserFound)
        parseStatement(tokens) match {
          case x@Some(_) =>
            parserFound = true  // Avoid running through all the parsers
            x :: Nil
          case None =>
            Nil
        }
      else
        Nil
    } collectFirst {
      case Some((parseResult, Nil)) =>
        parseResult :: Nil
      case Some((parseResult, remainingTokens)) =>
        parseResult +: parse(remainingTokens)
    } match {
      case Some(parseResult) => parseResult
      case None => Left(ParserNotFound(tokens)) :: Nil
    }
  }

  def errorOrExpression(tokens: Seq[Token]): Either[ParseError, Expression] = for {
    expressionTokens <- expr.expressionTokens(tokens)
    astRoot <- expr.toExpression(expr.toPostfix(expressionTokens))
  } yield astRoot
}

// "for" <var_ident> "in" <expr> ".." <expr> "do"
//     <stmts> "end" "for"
case class ForLoop(identifierToken: IdentifierToken, from: Expression, to: Expression, statements: Seq[StatementSequence]) extends StatementSequence

object ForLoop {
  def parse(tokens: Seq[Token]): Option[(ParseResult, Seq[Token])] =
    tokens match {
      case (_: ForKeyword) +: second +: third +: `l..r`((fromTokenCandidates, rightFromRangeToken)) =>
        rightFromRangeToken match {
          case `l<do>r`(toTokenCandidates, rightFromDo) =>
            rightFromDo match {
              case `l<end for;>r`(forLoopBodyStatements, unconsumedTokens) =>
              case class StatementSeq(statements: Seq[StatementSequence])
                val statements: Either[ParseError, StatementSeq] = frontendHelper
                  .errorsOrStatements(StatementSequence.parse(forLoopBodyStatements))
                  .left.map(ManyErrors)
                  .right.map(StatementSeq)

                val errorOrStatement = identifierOrError(second, tokens) ::
                  inKeywordOrError(third, tokens) ::
                  errorOrExpression(fromTokenCandidates) ::
                  errorOrExpression(toTokenCandidates) ::
                  statements ::
                  Nil match {
                    case Right(identifier: IdentifierToken) +: Right(_) +: Right(fromExpression: Expression) +: Right(toExpression: Expression) +: Right(loopStatements: StatementSeq) +: _ =>
                      Right(ForLoop(identifier, fromExpression, toExpression, loopStatements.statements))
                    case xs =>
                      Left(ManyErrors(xs.collect {
                        case Left(parseError) => parseError
                      }))
                  }
                Some(errorOrStatement, unconsumedTokens)
              case _ => None
            }
          case _ => None
        }
      case _ =>
        None
    }

}

// <var_ident> ":=" <expr>
case class VarAssignment(identifierToken: IdentifierToken, expression: Expression) extends StatementSequence

object VarAssignment {
  def parse(tokens: Seq[Token]): Option[(ParseResult, Seq[Token])] =
    tokens match {
      case (identifierToken: IdentifierToken) +: (_: AssignmentToken) +: `l;r`((leftOfSemicolon, rightOfSemicolon)) =>
        Some((errorOrExpression(leftOfSemicolon).map(expression => VarAssignment(identifierToken, expression)), rightOfSemicolon))
      case _ =>
        None
    }
}

// "print" <expr>
case class Print(expression: Expression) extends StatementSequence

object Print {
  def parse(tokens: Seq[Token]): Option[(ParseResult, Seq[Token])] =
    tokens match {
      case (_: PrintKeyword) +: `l;r`((leftOfSemicolon, rightOfSemicolon)) =>
        Some((errorOrExpression(leftOfSemicolon).map(Print(_)), rightOfSemicolon))
      case _ =>
        None
    }
}

// "var" <var_ident> ":" <type> [ ":=" <expr> ]
case class VarDeclaration(identifierToken: IdentifierToken, typeKeyword: TypeKeyword, expression: Option[Expression]) extends StatementSequence

object VarDeclaration {
  def parse(tokens: Seq[Token]): Option[(ParseResult, Seq[Token])] =
    tokens match {
      case (_: VarKeyword) +: second +: third +: fourth +: (_: SemicolonToken) +: tail => // // "var" <var_ident> ":" <type>
        val errorOrVarStatement = identifierOrError(second, tokens) :: typePrefixOrError(third, tokens) :: typeOrError(fourth,tokens) :: Nil match {
          case Right(identifier: IdentifierToken) +: Right(_) +: Right(typeVal: TypeKeyword) +: Nil =>
            Right(VarDeclaration(identifier, typeVal, None))
          case xs =>
            Left(ManyErrors(xs.collect {
              case Left(parseError) => parseError
            }))
        }
        Some((errorOrVarStatement, tail))
      case (_: VarKeyword) +: second +: third +: fourth +: fifth +: `l;r`((leftOfSemicolon, rightOfSemicolon))  => // "var" <var_ident> ":" <type> ":=" <expr>
        val assignmentOrError: Either[ParseError, Token] = fifth match {
          case assignment: AssignmentToken => Right(assignment)
          case wrongToken => Left(SyntaxError(tokens, s"$wrongToken is not the expected $AssignmentToken"))
        }

        val errorOrVarStatement = identifierOrError(second, tokens) :: typePrefixOrError(third, tokens) :: typeOrError(fourth,tokens) :: assignmentOrError :: errorOrExpression(leftOfSemicolon) :: Nil match {
          case Right(identifier: IdentifierToken) +: Right(_) +: Right(typeVal: TypeKeyword) +: Right(_) +: Right(expression: Expression) +: _ =>
            Right(VarDeclaration(identifier, typeVal, Some(expression)))
          case xs =>
            Left(ManyErrors(xs.collect {
              case Left(parseError) => parseError
            }))
        }
        Some((errorOrVarStatement, rightOfSemicolon))
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

  def inKeywordOrError(token: Token, tokens: Seq[Token]): Either[ParseError, Unit] = token match {
    case InKeyword(_) => Right(Unit)
    case wrongToken => Left(SyntaxError(tokens, s"Unexpected $wrongToken, expected $InKeyword"))
  }


  def typeOrError(token: Token, tokens: Seq[Token]): Either[ParseError, TypeKeyword] = token match {
    case typeKeyword: TypeKeyword => Right(typeKeyword)
    case wrongToken => Left(SyntaxError(tokens, s"Unexpected type keyword $wrongToken"))
  }

  val `l;r` = LeftRightSplitter(";")
  val `l..r` = LeftRightSplitter("..")
  val `l<do>r` = LeftRightSplitter("do")

  object `l<end for;>r` {
    def unapply(tokens: Seq[Token]): Option[(Seq[Token], Seq[Token])] = {
      tokens.zipWithIndex.collectFirst {
        case (_: EndKeyword, index) if tokens.slice(index + 1, index + 3).map(_.token) == Seq("for", ";") =>
          (tokens.take(index), tokens.drop(index + 3))
      }
    }
  }

  case class LeftRightSplitter(separator: String) {
    def unapply(tokens: Seq[Token]): Option[(Seq[Token], Seq[Token])] =
      tokens.zipWithIndex.collectFirst {
        case (t, index) if t.token == separator =>
          (tokens.take(index), tokens.drop(index + 1))
      }
  }
}
