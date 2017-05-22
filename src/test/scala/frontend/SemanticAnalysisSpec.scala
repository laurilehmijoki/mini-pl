package frontend

import frontend.Token._
import org.specs2.mutable.Specification

class SemanticAnalysisSpec extends Specification {

  implicit val tokenLocation = TokenLocation(-1)

  Seq(
    (
      """
        |var foo : int := 42;
        |var foo : int := 99;
      """.stripMargin.trim,
      TokenAlreadyDeclared(
        VarDeclaration(IdentifierToken("foo"),
          IntTypeKeyword("int"),
          OperandNode(IntToken(42))
        )
      )
    )
  ) foreach Function.tupled { (statementString, expectedError: SemanticError) =>
    s"semantic analyzer" should {
      s"find duplicate declaration error in '$statementString'" in {
        val tokens = Token.tokenize(statementString)
        val statements: Seq[StatementSequence] = StatementSequence.parse(tokens).map(_.right.get)
        val analysis = SemanticAnalysis.verify(statements)
        analysis match {
          case (alreadyDeclared: TokenAlreadyDeclared) :: Nil => alreadyDeclared should equalTo(expectedError)
        }
      }
    }
  }
}
