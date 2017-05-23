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
    ),
    (
      """
        |var foo : int := 2;
        |var foo : int := 3;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[IdentifierAlreadyDeclared])
      }: PartialFunction[VerificationResult, MatchResult[_]]
    ),
    (
      """
        |var z : string := 3;
        |var foo : int := 1 + z;
        |print foo;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[IncompatibleTypes])
      }: PartialFunction[VerificationResult, MatchResult[_]]
    ),
    (
      """
        |var z : int := 3;
        |var foo : string := 1 + z;
        |print foo;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[IncompatibleTypes])
      }: PartialFunction[VerificationResult, MatchResult[_]]
    ),
    (
      """
        |var z : int := 1 + 2 * 3 * 4;
        |var foo : int := 1 + z;
        |print foo;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(3)
      }: PartialFunction[VerificationResult, MatchResult[_]]
    )
  ) foreach Function.tupled { (program, x) =>
    s"verifier" should {
      s"correctly verify $program" in {
        frontendHelper.verify(program) should beLike(x)
      }
    }
  }

}
