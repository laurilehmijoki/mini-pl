package frontend

import org.specs2.mutable.Specification
import samples.samplePrograms

class VerifiedProgramSpec extends Specification {

  samplePrograms.programs foreach Function.tupled { (programDescription, sourceCode, verifier) =>
    s"verifier" should {
      s"correctly verify $sourceCode in ${programDescription.toLowerCase()}" in {
        frontendHelper.verify(sourceCode) should beLike(verifier)
      }
    }
  }

}
