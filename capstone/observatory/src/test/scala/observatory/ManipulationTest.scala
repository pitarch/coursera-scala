package observatory

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should

trait ManipulationTest extends MilestoneSuite with AnyFunSuiteLike with should.Matchers {
  private val milestoneTest = namedMilestoneTest("data manipulation", 4) _

  // Implement tests for methods of the `Manipulation` object

  test("makeGrid must return a grid whose predicted temperatures are consistent with the known temperatures") {
    val temperatures = Vector((Location(90, -180), 1d))

    val fn = Manipulation.makeGrid(temperatures)
    fn(GridLocation(90, -180)) === 1d
  }
}
