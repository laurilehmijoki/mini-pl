package samples

import frontend.{IdentifierAlreadyDeclared, IncompatibleTypes, InvalidExpression, ParserNotFound}
import frontend.frontendHelper.VerificationResult
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

object samplePrograms extends Specification {
  type SampleProgram = (String, String, PartialFunction[VerificationResult, MatchResult[_]])
  val programs: Seq[SampleProgram] = Seq(
    (
      "A program with unrecognised statement",
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
      "A program with duplicate var declarations",
      """
        |var foo : int := 2;
        |var foo : int := 3;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[IdentifierAlreadyDeclared])
      }: PartialFunction[VerificationResult, MatchResult[_]]
    ),
    (
      "A program where the user assigns an integer into string",
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
      "A program where the user assigns the value of the integer identifier to a string identifier",
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
      "A correct program",
      """
        |var z : int := 1 + 2 * 3 * 4;
        |var foo : int := 1 + z;
        |print foo;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(3)
      }: PartialFunction[VerificationResult, MatchResult[_]]
    )
  )
}
