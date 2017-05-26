package frontend

import scala.util.Try

case class TokenLocation(startIndex: Int)

sealed trait Token {
  def token: String
  def tokenLocation: TokenLocation

  override def toString = s"${getClass.getSimpleName}($token)"
}

object Token {
  val intToken = "(\\d+)".r
  val stringToken = """("(.*)")""".r
  val identifierToken = "([a-zA-Z]+)".r
  val operators = "+" :: "-" :: "*" :: "/" :: "<" :: "=" :: "&" :: "!" :: Nil

  case class UnidentifiedToken(string: String, startIndex: Int)

  def tokenize(source: String): Seq[Token] = {
    type CurrentToken = Option[String]
    val unidentifiedTokens: Seq[UnidentifiedToken] = source.zipWithIndex.foldLeft((Nil, None): (Seq[UnidentifiedToken], CurrentToken)) { (memo, charAndPosition) =>
      val chr: Char = charAndPosition._1
      val position = charAndPosition._2
      val whitespace = "(\\s)".r
      val currentTokenOptional: CurrentToken = memo._2
      lazy val accumulatedToken = currentTokenOptional.map(currentToken => UnidentifiedToken(currentToken, position - currentToken.length))
      val previousCandidates = memo._1
      def resolveWith(token: String) = {
        val previous = accumulatedToken.map(previousCandidates :+ _).getOrElse(previousCandidates)
        val operatorStartPosition = position + accumulatedToken.map(_.startIndex).getOrElse(0) - token.length
        (
          previous :+ UnidentifiedToken(token, operatorStartPosition),
          None
        )
      }

      chr.toString match {
        case whitespace(_) =>
          val lexemes = accumulatedToken.map(previousCandidates :+ _).getOrElse(previousCandidates)
          (
            lexemes,
            None
          )
        case _@"=" if currentTokenOptional.contains(":") => // the ":=" token
          val assignmentToken = ":="
          (
            previousCandidates :+ UnidentifiedToken(assignmentToken, position - assignmentToken.length),
            None
          )
        case operator if operators.contains(operator) =>
          resolveWith(operator)
        case terminator@";" =>
          resolveWith(terminator)
        case str =>
          (
            previousCandidates,
            currentTokenOptional.map(_ + str).orElse(Some(str))
          )
      }
    }._1

    unidentifiedTokens.map(Token.from)
  }

  def from(tokenCandidate: UnidentifiedToken): Token = {
    implicit val tokenLocation = TokenLocation(tokenCandidate.startIndex)
    tokenCandidate.string match {
      case t@"var" => VarKeyword(t)
      case t@"print" => PrintKeyword(t)
      case t@"int" => IntTypeKeyword(t)
      case t@"string" => StringTypeKeyword(t)
      case intToken(intCandidate) => Try(intCandidate.toInt).toOption.map(IntToken(_)).getOrElse(Unrecognised(intCandidate))
      case stringToken(token, containedString) => StringToken(token, containedString)
      case identifierToken(identifier) => IdentifierToken(identifier)
      case t@":=" => AssignmentToken(t)
      case t@";" => SemicolonToken(t)
      case t@":" => TypePrefixToken(t)
      case t@"+" => Plus(t)
      case t@"-" => Minus(t)
      case t@"*" => Multiply(t)
      case t@"/" => Divide(t)
      case unrecognised => Unrecognised(unrecognised)
    }
  }

  case class Unrecognised(token: String)(implicit val tokenLocation: TokenLocation) extends Token

  sealed trait StatementTerminator extends Token

  case class SemicolonToken(token: String)(implicit val tokenLocation: TokenLocation) extends StatementTerminator

  case class TypePrefixToken(token: String)(implicit val tokenLocation: TokenLocation) extends Token

  sealed trait ExpressionToken extends Token
  sealed trait OperatorToken extends ExpressionToken
  sealed trait OperandToken extends ExpressionToken

  sealed trait Terminal

  case class Plus(token: String)(implicit val tokenLocation: TokenLocation) extends OperatorToken
  case class Minus(token: String)(implicit val tokenLocation: TokenLocation) extends OperatorToken
  case class Multiply(token: String)(implicit val tokenLocation: TokenLocation) extends OperatorToken
  case class Divide(token: String)(implicit val tokenLocation: TokenLocation) extends OperatorToken
  case class IdentifierToken(token: String)(implicit val tokenLocation: TokenLocation) extends OperandToken
  case class IntToken(intValue: Int)(implicit val tokenLocation: TokenLocation) extends OperandToken with Terminal {
    override def token: String = intValue.toString
  }

  case class StringToken(token: String, containedString: String)(implicit val tokenLocation: TokenLocation) extends OperandToken with Terminal

  sealed trait Keyword extends Token
  case class VarKeyword(token: String)(implicit val tokenLocation: TokenLocation) extends Keyword
  case class PrintKeyword(token: String)(implicit val tokenLocation: TokenLocation) extends Keyword
  sealed trait TypeKeyword extends Keyword
  case class IntTypeKeyword(token: String)(implicit val tokenLocation: TokenLocation) extends TypeKeyword
  case class StringTypeKeyword(token: String)(implicit val tokenLocation: TokenLocation) extends TypeKeyword

  case class AssignmentToken(token: String)(implicit val tokenLocation: TokenLocation) extends Token
}
