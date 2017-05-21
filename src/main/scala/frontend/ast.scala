package frontend

import frontend.Token._

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait AstNode
case class OperandNode(operandToken: OperandToken) extends AstNode
case class OperatorNode(operatorToken: OperatorToken, left: AstNode, right: AstNode) extends AstNode

object ast {

  /**
    * https://en.wikipedia.org/wiki/Shunting-yard_algorithm
    */
  def toPostfix(infixTokens: Seq[ExpressionToken]): List[ExpressionToken] = {
    val outputStack = mutable.Stack[ExpressionToken]()
    val operatorStack = mutable.Stack[OperatorToken]()
    def precedence(operatorToken: OperatorToken): Int =
      operatorToken match {
        case Plus | Minus => 2
        case Multiply | Divide => 3
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

  def toPostfix(astNode: AstNode): String =
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
          case (Plus | Minus, first, second) => s"$second ${operator.token} $first"
          case (Multiply | Divide, first, second) => s"($second) ${operator.token} ($first)"
        }
        stack push str
    }

    stack.mkString
  }

  def main(args: Array[String]) = {
    //println(toInfix(IntToken(1) :: IntToken(2) :: Plus :: IntToken(3) :: IntToken(4) :: Divide :: Plus :: Nil))
    val postfix = toPostfix(IntToken(1) :: IntToken(2) :: Plus :: IntToken(3) :: IntToken(4) :: Divide :: Plus :: Nil)
    val x = toPostfix(ast.toAst(postfix).right.get)
    println(x)
  }

  @tailrec
  def toAst(postfixTokens: List[ExpressionToken], stack: List[AstNode] = Nil): Either[ParseError, AstNode] =
    postfixTokens match {
      case headToken +: tail =>
        headToken match {
          case operand: OperandToken => toAst(tail, OperandNode(operand) +: stack)
          case identifier: IdentifierToken => toAst(tail, OperandNode(VarReference(identifier)) +: stack)
          case operator: OperatorToken =>
            stack match {
              case first +: second +: stackTail =>
                toAst(tail, OperatorNode(operator, second, first) +: stackTail)
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

  def eval(rootNode: AstNode, statements: Seq[StatementSequence]): Int =
    rootNode match {
      case operandNode: OperandNode =>
        operandNode.operandToken match {
          case intToken: IntToken => intToken.intValue
          case varReference: VarReference =>
            val referencedAst = statements.collect {
              case v: VarDeclaration if v.identifierToken == varReference.identifierToken => v.expression
            }.headOption

            eval(referencedAst.getOrElse(throw new RuntimeException(s"Cannot find referenced var $varReference")), statements)
        }
      case operatorNode: OperatorNode =>
        val left = eval(operatorNode.left, statements)
        val right = eval(operatorNode.right, statements)
        operatorNode.operatorToken match {
          case Plus => left + right
          case Minus => left - right
          case Divide => left / right
          case Multiply => left * right
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
