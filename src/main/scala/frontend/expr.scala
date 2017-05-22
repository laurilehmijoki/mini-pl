package frontend

import frontend.Token._

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait Expression
case class OperandNode(operandToken: OperandToken) extends Expression
case class OperatorNode(operatorToken: OperatorToken, left: Expression, right: Expression) extends Expression

object expr {

  /**
    * https://en.wikipedia.org/wiki/Shunting-yard_algorithm
    */
  def toPostfix(infixTokens: Seq[ExpressionToken]): List[ExpressionToken] = {
    val outputStack = mutable.Stack[ExpressionToken]()
    val operatorStack = mutable.Stack[OperatorToken]()
    def precedence(operatorToken: OperatorToken): Int =
      operatorToken match {
        case Plus(_) | Minus(_) => 2
        case Multiply(_) | Divide(_) => 3
      }
    infixTokens.foreach {
      case identifier: IdentifierToken =>
        outputStack push identifier
      case operand: OperandToken =>
        outputStack push operand
      case operator: OperatorToken =>
        while (operatorStack.headOption.exists(precedence(_) >= precedence(operator))) {
          outputStack.push(operatorStack.pop())
        }
        operatorStack push operator
    }
    outputStack.pushAll(operatorStack).toList.reverse
  }

  def toPostfix(astNode: Expression): String =
    astNode match {
      case operand: OperandNode =>
        operand.operandToken.token
      case operator: OperatorNode =>
        s"${toPostfix(operator.left)} ${toPostfix(operator.right)} ${operator.operatorToken.token}"
    }

  def toInfix(postfix: Seq[ExpressionToken]): String = {
    val stack = mutable.Stack[String]()
    postfix.foreach {
      case identifierToken: IdentifierToken =>
        stack push identifierToken.token
      case operand: OperandToken =>
        stack push operand.token
      case operator: OperatorToken =>
        val str = (operator, stack.pop(), stack.pop()) match {
          case (Plus(_) | Minus(_), first, second) => s"$second ${operator.token} $first"
          case (Multiply(_) | Divide(_), first, second) => s"($second) ${operator.token} ($first)"
        }
        stack push str
    }

    stack.mkString
  }

  @tailrec
  def toExpression(postfixTokens: List[ExpressionToken], stack: List[Expression] = Nil): Either[ParseError, Expression] =
    postfixTokens match {
      case headToken +: tail =>
        headToken match {
          case operand: OperandToken => toExpression(tail, OperandNode(operand) +: stack)
          case operator: OperatorToken =>
            stack match {
              case first +: second +: stackTail =>
                toExpression(tail, OperatorNode(operator, second, first) +: stackTail)
              case _ =>
                Left(OperatorAtInvalidPosition(stack, operator))
            }
        }
      case Nil =>
        stack match {
          case rootNode +: Nil => Right(rootNode)
          case _ => Left(MalformedStack(stack))
        }
    }

  def expressionTokens(tokens: Seq[Token]): Either[ParseError, Seq[ExpressionToken]] =
    tokens.foldLeft(Right(Seq()): Either[ParseError, Seq[ExpressionToken]]) { (memo, token) =>
      memo.right.flatMap { expressionTokens =>
        token match {
          case e: ExpressionToken => Right(expressionTokens :+ e)
          case t => Left(SimpleError(s"Invalid token $t. Expected an expression token."))
        }
      }
    }
}
