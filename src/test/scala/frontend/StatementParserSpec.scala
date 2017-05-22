package frontend

import frontend.Token._
import org.specs2.mutable.Specification

class StatementParserSpec extends Specification {
  implicit val tokenLocation = TokenLocation(-1)

  Seq(
      (
        "var foo : int := 42;",
        VarDeclaration(
          IdentifierToken("foo"),
          IntTypeKeyword("int"),
          OperandNode(IntToken(42))
        )
      ),
      (
        "var foo : int := 1 + 2 * 3;",
        VarDeclaration(IdentifierToken("foo"),
          IntTypeKeyword("int"),
          OperatorNode(Plus(), OperandNode(IntToken(1)), OperatorNode(Multiply(), OperandNode(IntToken(2)), OperandNode(IntToken(3))))
        )
      ),
      (
        "print 42;",
        Print(OperandNode(IntToken(42)))
      )
    ) foreach Function.tupled { (statementString, statementSequence: StatementSequence) =>
      s"statement parser" should {
        s"parse '$statementString' correctly" in {
          val tokens = Token.tokenize(statementString)
          StatementSequence.parse(tokens) match {
            case head +: Nil => head.right.get should equalTo(statementSequence)
          }
        }
      }
    }
}
