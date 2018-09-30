package solutions

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import Solutions._

class SolutionsSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  it("Find the last element of a list.") {
    forAll(arbitrary[List[String]]) { xs =>
      head(xs) shouldBe xs.lastOption
    }
  }

  it("Find the last but one element of a list.") {
    forAll(arbitrary[List[String]]) { xs =>
      if (xs.length >= 2) penultimate(xs) shouldBe Some(xs(xs.length - 1))
      else
        penultimate(xs) shouldBe None
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

      element(xs, index) should be(Some(xs(index)))
    }
  }

  it("Find the number of elements of a list.") {
    forAll(arbitrary[List[Int]]) { xs =>
      Solutions.size(xs) should be(xs.size)
    }
  }

  it("Reverse a list.") {
    forAll(arbitrary[List[String]]) { xs =>
      reverse(xs) should be(xs.reverse)
    }
  }

  it("Find out whether a list is a palindrome.") {
    forAll(arbitrary[List[String]]) { xs =>
      isPalindrome(xs) should be(xs == xs.reverse)
    }
  }

  it("Flatten a nested list structure.") {
    forAll(arbitrary[List[List[Int]]]) { xs =>
      flatten(xs) should be(xs.flatten)
    }

    flatten(List(List(1, 2, 3), 4, List(5, 6))) should be(List(1, 2, 3, 4, 5, 6))
  }

  it("Eliminate consecutive duplicates of list elements.") {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List('a, 'b, 'c, 'a, 'd, 'e))
  }

}
