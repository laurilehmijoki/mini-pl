package frontend

import org.specs2.mutable.Specification
import samples.samplePrograms

class VerifiedProgramSpec extends Specification {

  samplePrograms.programs foreach Function.tupled { (program, verifier) =>
    s"verifier" should {
      s"correctly verify $program" in {
        frontendHelper.verify(program) should beLike(verifier)
      }
    }
  }

}
