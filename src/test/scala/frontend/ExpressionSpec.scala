package frontend

import frontend.Token._
import interpreter.IntegerValue
import org.specs2.mutable.Specification
import interpreter.interpreter._

class ExpressionSpec extends Specification {

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
        val postfix = expr.toPostfix(expressionTokens)
        val astRoot = expr.toExpression(postfix).right.get
        evaluate(astRoot, Map()) should equalTo(IntegerValue(expectedEvalResult))
      }
    }
  }
}
