package samples

import frontend._
import frontend.frontendHelper.VerificationResult
import interpreter.{IntegerValue, StringValue, SymbolValue}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

object samplePrograms extends Specification {
  case class InterpretationResult(
                                 symbolTable: Map[String, SymbolValue],
                                 stdout: Option[String]
                                 )
  case class SampleProgram(
                            description: String,
                            sourceCode: String,
                            matcher: PartialFunction[VerificationResult, MatchResult[_]],
                            interpretationResult: Option[InterpretationResult],
                            stdIn: Option[() => String] = None
                          )
  val programs: Seq[SampleProgram] = Seq(
    SampleProgram(
      "A program with unrecognised statement",
      """
        |var z : int := 3;
        |var foo : int := 1 + "";
        |print foo;
        |baz
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[ParserNotFound])
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = None
    ),
    SampleProgram(
      "A program with duplicate var declarations",
      """
        |var foo : int := 2;
        |var foo : int := 3;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[IdentifierAlreadyDeclared])
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = None
    ),
    SampleProgram(
      "A program with var reassignment",
      """
        |var foo : int := 2;
        |foo := 3;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(2)
      }: PartialFunction[VerificationResult, MatchResult[_]],
      Some(InterpretationResult(
        Map(
          "foo" -> IntegerValue(3)
        ),
        stdout = None
      ))
    ),
    SampleProgram(
      "A program with illegal var reassignment",
      """
        |foo := 3;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[IdentifierNotDeclared])
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = None
    ),
    SampleProgram(
      "A program with var default value",
      """
        |var intIdentifier : int;
        |var stringIdentifier : string;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(2)
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = Some(InterpretationResult(
        Map(
          "intIdentifier" -> IntegerValue(0),
          "stringIdentifier" -> StringValue("")
        ),
        stdout = None
      ))
    ),
    SampleProgram(
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
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = None
    ),
    SampleProgram(
      "A program where the user assigns the value of the integer identifier to a string identifier",
      """
        |var z : int := 3;
        |var foo : string := z;
        |print foo;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[IncompatibleTypes])
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = None
    ),
    SampleProgram(
      "A correct program with integer arithmetics",
      """
        |var z : int := 1 + 2*3 * 4;
        |var foo : int := 1 + z;
        |print foo;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(3)
      }: PartialFunction[VerificationResult, MatchResult[_]],
      Some(InterpretationResult(
        Map(
          "z" -> IntegerValue(1 + 2 * 3 * 4),
          "foo" -> IntegerValue(1 + 1 + 2 * 3 * 4)
        ),
        stdout = Some(s"${1 + 1 + 2 * 3 * 4}")
      ))
    ),
    SampleProgram(
      "A correct program with string concatenation",
      """
        |var z : string := "foo";
        |var foo : string := z + "bar";
        |print foo;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(3)
      }: PartialFunction[VerificationResult, MatchResult[_]],
      Some(InterpretationResult(
        Map(
          "z" -> StringValue("foo"),
          "foo" -> StringValue("foobar")
        ),
        stdout = Some("foobar")
      ))
    ),
    SampleProgram(
      "A program with for loop",
      """
        |var z : int;
        |for z in 0..5 do
        |  print z;
        |end for;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(2)
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = Some(InterpretationResult(
        Map(
          "z" -> IntegerValue(4)
        ),
        stdout = Some("01234")
      ))
    ),
    SampleProgram(
      "A program with empty for loop body",
      """
        |var z : int;
        |for z in 0..5 do
        |end for;
        |""".stripMargin,
      {
        case Left((errors: ManyErrors) :: Nil) =>
          errors.errors.map(_.getClass) must equalTo(classOf[EmptyForLoopBody] :: Nil)
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = Some(InterpretationResult(
        Map(),
        stdout = None
      ))
    ),
    SampleProgram(
      "A program with for loop from string to int",
      """
        |var z : int;
        |for z in "test"..5 do
        |  print z;
        |end for;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[IncompatibleTypes])
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = Some(InterpretationResult(
        Map(),
        stdout = None
      ))
    ),
    SampleProgram(
      "A program with reassignment of the for loop control variable",
      """
        |var z : int;
        |for z in 2..3 do
        |  z := 5;
        |end for;
        |""".stripMargin,
      {
        case Left(error +: Nil) => error.getClass must equalTo(classOf[ControlVariableMayNotBeReassigned])
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = Some(InterpretationResult(
        Map(),
        stdout = None
      ))
    ),
    SampleProgram(
      "A program with var declaration within the for loop",
      """
        |var z : int;
        |for z in 2..3 do
        |  var y : int;
        |end for;
        |""".stripMargin,
      {
        case Left(error +: Nil) =>
          error.getClass must equalTo(classOf[VarDeclarationWithinForLoop])
      }: PartialFunction[VerificationResult, MatchResult[_]],
      interpretationResult = Some(InterpretationResult(
        Map(),
        stdout = None
      ))
    ),
    SampleProgram(
      "A program with stdin read",
      """
        |var z : int;
        |read z;
        |""".stripMargin,
      {
        case Right(verifiedProgram) => verifiedProgram.statements must haveLength(2)
      }: PartialFunction[VerificationResult, MatchResult[_]],
      stdIn = Some(() => "42"),
      interpretationResult = Some(InterpretationResult(
        Map("z" -> IntegerValue(42)),
        stdout = None
      ))
    )
  )
}
