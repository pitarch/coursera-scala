package observatory

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

trait VisualizationTest extends MilestoneSuite with AnyFunSuiteLike with should.Matchers with Checkers with SparkTestSupport {

  import observatory.Visualization.{interpolateColor, predictTemperature}
  import observatory.pittarck.MyVisualization.prediction.{calculateGreatCircleDistance, calculateRhoDiff}

  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  def latitudeToPixel(lat: Double): Int = lat.toInt - 90

  def longitudeToPixel(lon: Double): Int = lon.toInt + 180

  // //////////////////////////////////////////////////////////////////////////
  // calculateRhoDiff
  // //////////////////////////////////////////////////////////////////////////

  test("calculateRhoDiff: should be NaN when calculated in the same location") {
    val loc = Location(-8.0, -30.0)
    val result = calculateRhoDiff(loc, loc)
    result.isNaN shouldBe true
  }

  // //////////////////////////////////////////////////////////////////////////
  // calculateGreatCircleDistance
  // //////////////////////////////////////////////////////////////////////////

  test("calculateGreatCircleDistance: 1/20 latitude drift should produce a distance lower than 1km") {
    val loc1 = Location(0, 0)
    val loc2 = loc1.copy(lat = loc1.lat + 0.005)
    calculateGreatCircleDistance(loc1, loc2) should be < 1d
  }


  // //////////////////////////////////////////////////////////////////////////
  // predictTemperature
  // //////////////////////////////////////////////////////////////////////////

  test("predictTemperature: the same location where we have real measures") {
    val location = Location(2, 10)
    val measures = Stream((location, 10d))

    predictTemperature(measures, location) shouldEqual 10d
  }

  //Location(8.0,-38.0) with temperatures: List((Location(8.0,-38.0),26.117131062951486))
  test("predictTemperature: Location(8.0,-38.0) with temperatures: (Location(8.0,-38.0),26.117131062951486)") {

    val temperatures = Seq((Location(-8.0, -38.0), 26.117131062951486))
    val targetLocation = Location(-8.0, -38.0)
    predictTemperature(temperatures, targetLocation) === 26.117131062951486 +- 0.0001
  }

  test("predictTemperature: a closer location affect more to the temperature of the target location") {
    val m0 = (Location(10, 0), 10d)
    val m1 = (Location(40, 0), 20d)
    val target = Location(0, 0)
    val result = predictTemperature(Seq(m0, m1), target)
    result shouldBe 10d +- 0.1
  }

  test("predictTemperature: a location from a distance lower 1km from an station") {
    val location = Location(2, 10)
    val targetLocation = location.copy(lat = location.lat + 1 / 20)
    val measures = Stream((location, 10d))

    predictTemperature(measures, targetLocation) shouldEqual 10d
  }

  test("predictTemperature: in the antipodes from the station") {
    val location = Location(90, 0)
    val targetLocation = location.copy(lat = -location.lat)
    val measures = Stream((location, 10d))

    predictTemperature(measures, targetLocation) shouldEqual 10d
  }

  test("predicted temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa (10pts)") {

    val z = Location(0, 0)
    val x = Location(0, 45)
    val y = Location(0, 90)
    val temperatures = Seq((x, 10d), (y, 20d))
    val prediction = predictTemperature(temperatures, z)
    (prediction - 10d).abs should be < (prediction - 20d).abs
  }


  // //////////////////////////////////////////////////////////////////////////
  // interpolateColor
  // //////////////////////////////////////////////////////////////////////////

  test("interpolateColor: Expected to be closer to Color(0,0,255) than Color(255,0,0)") {

    val temp = 13.215867566016941
    val colors = List((10.0, Color(255, 0, 0)), (20.0, Color(0, 0, 255)))

    val result = interpolateColor(colors, temp)

    result shouldBe Color(173, 0, 82) // Expected to be closer to Color(0,0,255) than Color(255,0,0)
  }

  test("interpolateColor") {

    val colors = Seq(
      (0.0, Color(255, 0, 0)),
      (10.0, Color(0, 0, 255))
    )

    interpolateColor(colors, -1.0) shouldBe Color(255, 0, 0)
    interpolateColor(colors, 0.0) shouldBe Color(255, 0, 0)
    interpolateColor(colors, 5.0) shouldBe Color(128, 0, 128)
    interpolateColor(colors, 10.0) shouldBe Color(0, 0, 255)
    interpolateColor(colors, 11.0) shouldBe Color(0, 0, 255)
  }

  /*
[Test Description] visualize (5pts)(observatory.CapstoneSuite)
[Observed Error] Incorrect computed color at Location(90.0,-180.0): Color(26,0,229).
Expected to be closer to Color(255,0,0) than Color(0,0,255)
*/
  test("interpolateColor: Expected to be closer to Color(255,0,0) than Color(0,0,255) at Location(90.0,-180.0)") {

    val temperatures = Seq(
      (Location(45.0, -90.0), 0.0),
      (Location(-45.0, 0.0), -9.840616603588725)
    )

    val colors = Seq(
      (0.0, Color(255, 0, 0)),
      (-9.840616603588725, Color(0, 0, 255))
    )

    val predictedTemperature = predictTemperature(temperatures, Location(90.0, -180.0))
    val color = interpolateColor(colors, predictedTemperature)
    color shouldEqual Color(255, 0, 0)
  }


  test("interpolate color: case 1") {
    // [Test Description] tile pixel colors must be consistent with the given located temperatures and color scale (5pts)(observatory.CapstoneSuite)
    //
    // temperatures: List((Location(45.0,-90.0),10.0), (Location(-45.0,0.0),20.0))
    // colors: List((10.0,Color(255,0,0)), (20.0,Color(0,0,255)))
    // tile: Tile(0,0,0)
    //
    // Incorrect computed color at Location(-27.059125784374057,-180.0): Color(197,0,58).
    // Expected to be closer to Color(0,0,255) than Color(255,0,0)

    val loc10 = Location(45.0, -90.0)
    val loc20 = Location(-45.0, 0.0)
    val temperatures = List((loc10, 10.0), (loc20, 20.0))
    val colors = List((10.0, Color(255, 0, 0)), (20.0, Color(0, 0, 255)))
    val targetLocation = Location(-27.059125784374057, -180.0)
    //    val dist10 = MyVisualization.prediction.calculateGreatCircleDistance(targetLocation, loc10)
    //    val dist20 = MyVisualization.prediction.calculateGreatCircleDistance(targetLocation, loc20)

    val prediction = predictTemperature(temperatures, targetLocation)
    val color = Visualization.interpolateColor(colors, prediction)
    color.blue should be > color.red
  }


  test("interpolateColor: basic color interpolation") {
    import Visualization._

    val points = Seq((1.0, Color(255, 0, 0)), (2.0, Color(0, 0, 255)), (3.0, Color(0, 255, 0)))
    val target = 1.5
    interpolateColor(points, target) shouldEqual Color(128, 0, 128)
  }

  test("interpolateColor: color interpolation") {
    import Visualization._

    val points = Seq((-1.0, Color(255, 0, 0)), (0.0, Color(0, 0, 255)))
    val target = -0.5

    interpolateColor(points, target) shouldEqual Color(128, 0, 128)
  }


  test("interpolateColor: exceeding the greatest value of a color scale should return the color associated with the greatest value 1") {
    import Visualization._

    val points = List((-1.2017044414630357E-5, Color(255, 0, 0)), (0.0, Color(0, 0, 255)))
    val target = -1.2017044414630357E-5
    interpolateColor(points, target) shouldEqual Color(255, 0, 0)
  }

  test("interpolateColor: exceeding the greatest value of a color scale should return the color associated with the greatest value 2") {
    import Visualization._

    val points = Seq((0.0, Color(255, 0, 0)), (1.8655157632659994E-5, Color(0, 0, 255)))
    val target = 0.0

    interpolateColor(points, target) shouldEqual Color(255, 0, 0)
  }

  test("interpolateColor: exceeding the greatest value of a color scale should return the color associated with the greatest value 3") {
    import Visualization._

    val points = List((-1.544897167753534E-5, Color(255, 0, 0)), (0.0, Color(0, 0, 255)))
    val target = -10.000015448971677

    interpolateColor(points, target) shouldEqual Color(255, 0, 0)
  }
}
