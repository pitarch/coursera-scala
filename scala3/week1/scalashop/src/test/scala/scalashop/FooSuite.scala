package scalashop

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FooSuite extends AnyFunSuite with Matchers {


  test("foo") {
    val img = new Img(10, 10)
    val numTasks = 2
    val fromRange = (0 until img.height by numTasks)
    val stripRanges = fromRange zip fromRange.tail :+ img.height
    stripRanges shouldBe Seq((0, 3), (3, 6), (6,9))
  }
}
