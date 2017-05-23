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
          case t => Left(SyntaxError(tokens, s"Invalid token $t. Expected an expression token."))
        }
      }
    }
}
