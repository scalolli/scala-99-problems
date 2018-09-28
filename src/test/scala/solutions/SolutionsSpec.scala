package solutions

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class SolutionsSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  it("Find the last element of a list.") {
    forAll(arbitrary[List[String]]) { xs =>
      (Solutions.head(xs)) shouldBe xs.lastOption
    }
  }

  it("Find the last but one element of a list.") {
    forAll(arbitrary[List[String]]) { xs =>
      if (xs.length >= 2) Solutions.penultimate(xs) shouldBe Some(xs(xs.length - 1))
      else
        Solutions.penultimate(xs) shouldBe None
    }
  }

  it("Find the Kth element of a list.") {
    case class TestData(xs: List[String], index: Int)
    val genTestData = for {
      xs    <- Gen.nonEmptyListOf[String](arbitrary[String])
      index <- Gen.choose(0, xs.size - 1)
    } yield TestData(xs, index)

    forAll(genTestData) { testData =>
      import testData._

      Solutions.element(xs, index) should be(Some(xs(index)))
    }
  }

}
