package solutions

import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class SolutionsSpec
    extends FunSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  it("Find the last element of a list.") {
    forAll(arbitrary[List[String]]) { xs =>
      (Solutions.head(xs)) shouldBe xs.lastOption
    }
  }

  it("Find the last but one element of a list.") {
    forAll(arbitrary[List[String]]) { xs =>
      if(xs.length >= 2)
        (Solutions.penultimate(xs)) shouldBe Some(xs(xs.length - 1))
      else
        Solutions.penultimate(xs) shouldBe None
    }
  }
}
