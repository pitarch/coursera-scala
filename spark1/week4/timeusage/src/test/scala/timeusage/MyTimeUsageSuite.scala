package timeusage

import org.apache.spark.sql.Row
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class MyTimeUsageSuite extends AnyFunSuite with should.Matchers {

  test("row returns Row with the all columns being Double expect the first column") {

    val line = List("foo", "1.2", "0.03")
    val row = TimeUsage.row(line)
    row shouldBe Row("foo", 1.2d, 0.03d)
  }
}
