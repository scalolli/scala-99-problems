package solutions

import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class SolutionsSpec
    extends FunSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  it("should find the last element of a list") {
    forAll(arbitrary[List[String]]) { xs =>
      xs.lastOption shouldBe (Solutions.head(xs))
    }
  }

}
