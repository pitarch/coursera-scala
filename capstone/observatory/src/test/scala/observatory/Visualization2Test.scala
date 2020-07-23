package observatory

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

trait Visualization2Test extends MilestoneSuite with AnyFunSuiteLike with should.Matchers with Checkers {
  private val milestoneTest = namedMilestoneTest("value-added information visualization", 5) _

  // Implement tests for methods of the `Visualization2` object

  test("GridLocation.locationStream") {

    GridLocation.locationStream.length shouldBe (180 * 360)
  }

  //  test("grid visualization") {
  //    val colors = List((10.0, Color(255, 0, 0)), (20.0, Color(0, 0, 255)))
  //
  //    def gridFunc(gridLoc: GridLocation) = 10.0
  //
  //    val image = Visualization2.visualizeGrid(gridFunc, colors, Tile.TILE0)
  //
  //    image.pixel(0, 148).toColor shouldBe RGBColor(255, 0, 0, 127)
  //  }

}
