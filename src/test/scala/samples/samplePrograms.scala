package samples

import frontend._
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
      "A program with var reassignment",
      """
        |var foo : int := 2;
        |foo := 3;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(2)
      }: PartialFunction[VerificationResult, MatchResult[_]]
    ),
    (
      "A program with illegal var reassignment",
      """
        |foo := 3;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[IdentifierNotDeclared])
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
        case Left(first +: second +: Nil) =>
          first.getClass must equalTo(classOf[IncompatibleTypes])
          second.getClass must equalTo(classOf[InvalidExpression])
      }: PartialFunction[VerificationResult, MatchResult[_]]
    ),
    (
      "A program where the user assigns the value of the integer identifier to a string identifier",
      """
        |var z : int := 3;
        |var foo : string := z;
        |print foo;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[IncompatibleTypes])
      }: PartialFunction[VerificationResult, MatchResult[_]]
    ),
    (
      "A correct program with integer arithmetics",
      """
        |var z : int := 1 + 2 * 3 * 4;
        |var foo : int := 1 + z;
        |print foo;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(3)
      }: PartialFunction[VerificationResult, MatchResult[_]]
    ),
    (
      "A correct program with string concatenation",
      """
        |var z : string := "foo";
        |var foo : string := z + "bar";
        |print foo;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(3)
      }: PartialFunction[VerificationResult, MatchResult[_]]
    )
  )
}
