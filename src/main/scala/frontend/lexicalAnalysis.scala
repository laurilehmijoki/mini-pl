package frontend

import scala.util.Try

sealed trait Token {
  def token: String

  override def toString = s"${getClass.getSimpleName}($token)"
}

object Token {
  val intToken = "(\\d+)".r
  val identifierToken = "([a-zA-Z]+)".r

  def tokenize(str: String): Seq[Token] =
    str.trim.split("\\s")
      .flatMap { str =>
        val itemWithTerminator = "(.*)(;)".r
        str match {
          case itemWithTerminator(item, terminator) => Seq(item, terminator)
          case _ => Seq(str)
        }
      }
      .map(Token.from)


  def from(tokenCandidate: String): Token =
    tokenCandidate match {
      case VarKeyword.token => VarKeyword
      case PrintKeyword.token => PrintKeyword
      case IntTypeKeyword.token => IntTypeKeyword
      case StringTypeKeyword.token => StringTypeKeyword
      case intToken(intCandidate) => Try(intCandidate.toInt).toOption.map(IntToken).getOrElse(Unrecognised(intCandidate))
      case identifierToken(identifier) => IdentifierToken(identifier)
      case AssignmentToken.token => AssignmentToken
      case SemicolonToken.token => SemicolonToken
      case TypePrefixToken.token => TypePrefixToken
      case Plus.token => Plus
      case Minus.token => Minus
      case Multiply.token => Multiply
      case Divide.token => Divide
      case x => Unrecognised(x)
    }

  case class Unrecognised(token: String) extends Token

  sealed trait StatementTerminator extends Token

  object SemicolonToken extends StatementTerminator {
    override val token: String = ";"
  }

  object TypePrefixToken extends Token {
    override val token: String = ":"
  }

  sealed trait ExpressionToken extends Token
  sealed trait OperatorToken extends ExpressionToken
  sealed trait OperandToken extends ExpressionToken

  object Plus extends OperatorToken {
    override val token: String = "+"
  }

  object Minus extends OperatorToken {
    override val token: String = "-"
  }

  object Multiply extends OperatorToken {
    override val token: String = "*"
  }

  object Divide extends OperatorToken {
    override val token: String = "/"
  }

  case class IntToken(intValue: Int) extends OperandToken {
    override def token: String = intValue.toString
  }

  sealed trait Keyword extends Token

  object VarKeyword extends Keyword {
    override val token: String = "var"
  }

  object PrintKeyword extends Keyword {
    override val token: String = "print"
  }

  sealed trait TypeKeyword extends Keyword

  object IntTypeKeyword extends TypeKeyword {
    override val token: String = "int"
  }

  object StringTypeKeyword extends TypeKeyword {
    override val token: String = "string"
  }

  case class IdentifierToken(token: String) extends OperandToken

  object AssignmentToken extends Token {
    override val token: String = ":="
  }
}
