package frontend

import frontend.Token._
import org.specs2.mutable.Specification

class AstSpec extends Specification {

 Seq(
   (
     "1 - 2",
     1 - 2
   ),
   (
     "1 - 2 - 2",
     1 - 2 - 2
   ),
   (
      "1 + 2",
      1 + 2
    ),
    (
      "2 * 2 + 2",
      2 * 2 + 2
    ),
    (
      "2 + 2 * 2",
      2 + 2 * 2
    ),
    (
      "2 + 2 * 2 + 3",
      2 + 2 * 2 + 3
    ),
    (
      "2 + 2 * 2 + 3 + 3",
      2 + 2 * 2 + 3 + 3
    ),
    (
      "1 + 2 * 3 * 4",
      1 + 2 * 3 * 4
    ),
    (
      "1 + 2 + 3 * 4 * 5 + 6 + 7 * 8",
      1 + 2 + 3 * 4 * 5 + 6 + 7 * 8
    ),
   (
     "1 + 2 + 3 * 4 * 5 + 6 + 7 * 8 / 2",
     1 + 2 + 3 * 4 * 5 + 6 + 7 * 8 / 2
   )
  ) foreach Function.tupled { (expressionString: String, expectedEvalResult: Int) =>
    expressionString should {
      s"equal to $expectedEvalResult" in {
        val expressionTokens = Token.tokenize(expressionString).collect {
          case expressionToken: ExpressionToken => expressionToken
        }
        val postfix = ast.toPostfix(expressionTokens)
        val astRoot = ast.toAst(postfix).right.get
        eval(astRoot, Nil) should equalTo(expectedEvalResult)
      }
    }
  }

  Seq(
    "1 * *"
  ) foreach { invalidExpression =>
    invalidExpression should {
      s"result in an error" in {
        val expressionTokens = Token.tokenize(invalidExpression).collect {
          case expressionToken: ExpressionToken => expressionToken
        }
        ast.toAst(ast.toPostfix(expressionTokens)).left.get match {
          case invalidStack: OperatorAtInvalidPosition =>
            invalidStack.operatorToken should equalTo(Token.Multiply)
        }
      }
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
}
