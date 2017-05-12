package frontend

import frontend.Token.ExpressionToken
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
        ast.eval(astRoot) should equalTo(expectedEvalResult)
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
}
