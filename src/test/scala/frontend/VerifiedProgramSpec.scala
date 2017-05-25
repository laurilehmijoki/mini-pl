package frontend

import org.specs2.mutable.Specification
import samples.samplePrograms

class VerifiedProgramSpec extends Specification {

  samplePrograms.programs foreach { sampleProgram =>
    s"verifier" should {
      s"correctly verify ${sampleProgram.sourceCode} in ${sampleProgram.description.toLowerCase()}" in {
        frontendHelper.verify(sampleProgram.sourceCode) should beLike(sampleProgram.matcher)
      }
    }
  }

}
