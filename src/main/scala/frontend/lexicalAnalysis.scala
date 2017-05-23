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

  case class TokenCandidate(string: String, startIndex: Int)

  def tokenize(source: String): Seq[Token] = {
    type CurrentToken = Option[String]
    val tokenCandidates: Seq[TokenCandidate] = source.zipWithIndex.foldLeft((Nil, None): (Seq[TokenCandidate], CurrentToken)) { (memo, charAndPosition) =>
      val chr: Char = charAndPosition._1
      val position = charAndPosition._2
      val whitespace = "(\\s)".r
      val currentToken: CurrentToken = memo._2
      val previousCandidates = memo._1
      chr.toString match {
        case whitespace(space) =>
          val candidates = currentToken
            .map { currentTokenStr =>
              previousCandidates :+
                TokenCandidate(currentTokenStr, position - currentTokenStr.length)
            }.getOrElse(previousCandidates)
          (
            candidates,
            None
          )
        case str =>
          (
            previousCandidates,
            currentToken.map(_ + str).orElse(Some(str))
          )
      }
    }._1.flatMap { candidate =>
      val tokenWithStatementTerminator = "(.+)(;)".r
      candidate.string match {
        case tokenWithStatementTerminator(token, terminator) =>
          TokenCandidate(token, candidate.startIndex) :: TokenCandidate(terminator, candidate.startIndex + token.length) :: Nil
        case _ =>
          candidate :: Nil
      }
    }

    tokenCandidates.map(Token.from)
  }

  def from(tokenCandidate: TokenCandidate): Token = {
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
      case "+" => Plus()
      case "-" => Minus()
      case "*" => Multiply()
      case "/" => Divide()
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

  case class Plus(implicit val tokenLocation: TokenLocation) extends OperatorToken {
    override def token: String = "+"
  }
  case class Minus(implicit val tokenLocation: TokenLocation) extends OperatorToken {
    override def token: String = "-"
  }
  case class Multiply(implicit val tokenLocation: TokenLocation) extends OperatorToken {
    override def token: String = "*"
  }
  case class Divide(implicit val tokenLocation: TokenLocation) extends OperatorToken {
    override def token: String = "/"
  }
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
