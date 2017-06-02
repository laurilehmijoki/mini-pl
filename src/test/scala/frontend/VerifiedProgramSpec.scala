package frontend

import java.io.{ByteArrayOutputStream, PrintStream}

import org.specs2.mutable.Specification
import samples.samplePrograms
import utils.{BashShell, FormattingContext}

class VerifiedProgramSpec extends Specification {

  samplePrograms.programs.foreach { sampleProgram =>
    val errorOrVerifiedProgram = frontendHelper.verify(sampleProgram.sourceCode)

    s"verifier" should {
      s"correctly verify ${sampleProgram.sourceCode} in ${sampleProgram.description.toLowerCase()}" in {
        errorOrVerifiedProgram should beLike(sampleProgram.matcher)
      }
    }

    for {
      verifiedProgram <- errorOrVerifiedProgram.right.toOption
      interpretationResult <- sampleProgram.interpretationResult.ensuring(_.isDefined /* if the program is verified, expect there to be an interpretation result*/)
    } {
      s"interpretation of ${sampleProgram.sourceCode} in ${sampleProgram.description.toLowerCase()}" should {
        implicit val formattingContext: FormattingContext = BashShell
        val baos = new ByteArrayOutputStream()
        implicit val stdOut: PrintStream = new PrintStream(baos)
        implicit val stdIn: (() => String) = sampleProgram.stdIn.getOrElse(() => throw new RuntimeException("No stdin provided"))
        val symbolTable = interpreter.interpreter.interpret(verifiedProgram)

        s"result in correct symbol table" in {
          symbolTable should equalTo(interpretationResult.symbolTable)
        }

        s"result in correct standard output" in {
          val actualStdout = new String(baos.toByteArray, "utf-8")
          interpretationResult.stdout should beLike {
            case Some(expectedStdout) => actualStdout must equalTo(expectedStdout)
            case None => actualStdout must equalTo("")
          }
        }
      }
    }
  }
}
