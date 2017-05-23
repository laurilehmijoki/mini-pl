package frontend

import frontend.frontendHelper.VerificationResult
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class VerifiedProgramSpec extends Specification {

  Seq(
    (
      """
        |var z : int := 3;
        |var foo : int := 1 + "";
        |print foo;
        |baz
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[ParserNotFound])
      }: PartialFunction[VerificationResult, MatchResult[_]]
    )
  ) foreach Function.tupled { (program, x) =>
    s"verifier" should {
      s"should correctly verify $program" in {
        frontendHelper.verify(program) should beLike(x)
      }
    }
  }

}
