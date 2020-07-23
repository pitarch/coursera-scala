package observatory.pittarck

import observatory.Location
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should

import scala.collection.mutable.ListBuffer

class MyTemperatureOrbitTest extends AnyFunSuiteLike with should.Matchers {

  import MyTemperatureOrbit._

  test("generateTopOrbits") {

    generateTopOrbits(2, 3, 1) shouldEqual Stream((1, 2), (1, 3), (1, 4))
  }

  test("generateBottomOrbits") {

    generateBottomOrbits(2, 3, 1) shouldEqual Stream((3, 2), (3, 3), (3, 4))
  }

  test("generateLateralOrbits") {
    generateLateralOrbits(2, 3, 1) shouldEqual Stream((2, 2), (2, 4))
  }

  test("streamOrbits") {
    streamOrbits((4, 5), 1).head._2 shouldEqual Stream(
      (3, 4), (3, 5), (3, 6),
      (4, 4), (4, 6),
      (5, 4), (5, 5), (5, 6)
    )
  }

  test("streamOrbits on the top-left edge") {
    streamOrbits((90, -180), 1).head._2 shouldEqual Stream((89, -180), (89, -179), (90, -179))
  }

  test("streamOrbits on the bottom-left edge") {
    streamOrbits((-90, -180), 1).head._2 shouldEqual Stream((-90, -179), (-89, -180), (-89, -179))
  }

  test("streamOrbits on the bottom-right edge") {
    streamOrbits((-90, 180), 1).head._2 shouldEqual Stream(
      (-90, 179),
      (-89, 179), (-89, 180)
    )
  }

  test("classify in the same bucket") {
    val temperatures = List((Location(1.1, 2.2), 10d), (Location(1.3, 2.2), 10d))
    val buckets = classify(temperatures)
    buckets should contain((1, 2) -> ListBuffer((Location(1.1, 2.2), 10d), (Location(1.3, 2.2), 10d)))
  }

  test("classify in different buckets") {
    val temperatures = List((Location(1.1, 2.2), 10d), (Location(1.1, 3.2), 10d))
    val buckets = classify(temperatures)
    buckets should contain key ((1, 2))
    buckets should contain key ((1, 3))
    buckets((1, 2)) shouldEqual List((Location(1.1, 2.2), 10d))
    buckets((1, 3)) shouldEqual List((Location(1.1, 3.2), 10d))
  }

  test("streamOrbitsWithBuckets: head give the first orbit with temperature measures") {
    val temperatures = List((Location(2, 2), 10d))
    val buckets = classify(temperatures)
    val center = (0, 0)
    streamOrbitsWithBuckets(buckets, center).head should equal((2, List((2, 2))))
  }

  test("streamOrbitsWithBuckets: should return empty when no more orbits with measures") {
    val buckets = classify(List.empty)
    val center = (0, 0)
    streamOrbitsWithBuckets(buckets, center) shouldBe empty
  }

  test("streamOrbitsWithBuckets: two orbits with measures") {
    val temperatures = List((Location(2, 2), 10d), (Location(4, 5), 10d))
    val buckets = classify(temperatures)
    val center = (0, 0)
    val stream = streamOrbitsWithBuckets(buckets, center)
    stream.head should equal((2, List((2, 2))))
    stream.tail.head should equal((5, List((4, 5))))
  }

  test("streamOrbitsWithBuckets: antipodes") {
    val temperatures = List((Location(90, 0), 10d))
    val buckets = classify(temperatures)
    val center = (-90, 0)
    val stream = streamOrbitsWithBuckets(buckets, center)
    stream.head should equal((180, Stream((90, 0))))
  }

  test("streamUntilCondition: use temperatures from the first orbit with measures") {

    val temperatures = List((Location(10, 0), 10d), (Location(20, 0), 10d))
    val buckets = classify(temperatures)
    val center = (0, 0)
    val meetCondition = (prevRadio: Int, radio: Int, count: Int) => count > 0
    val stream = streamUntilCondition(toTemperatureMeasuresByOrbit(streamOrbitsWithBuckets(buckets, center), buckets), meetCondition)
    stream.head should equal((10, Stream((Location(10, 0), 10d))))
    stream.tail shouldBe empty
  }

  test("streamUntilCondition: stream finishes when the space has been evaluated even though the condition is not meet") {

    val temperatures = List((Location(10, 0), 10d), (Location(20, 0), 10d))
    val buckets = classify(temperatures)
    val center = (0, 0)
    val meetCondition = (prevRadio: Int, radio: Int, count: Int) => count > 10
    val stream = streamUntilCondition(toTemperatureMeasuresByOrbit(streamOrbitsWithBuckets(buckets, center), buckets), meetCondition)
    stream.head should equal((10, Stream((Location(10, 0), 10d))))
    stream.tail.head should equal((20, Stream((Location(20, 0), 10d))))
    stream.tail.tail shouldBe empty
  }

  test("streamUntilCondition: should return only measures under a radio of 10 degrees") {

    val temperatures = List((Location(10, 0), 10d), (Location(20, 0), 10d))
    val buckets = classify(temperatures)
    val center = (0, 0)
    val meetCondition = (prevRadio: Int, radio: Int, count: Int) => radio > 10
    val stream = streamUntilCondition(toTemperatureMeasuresByOrbit(streamOrbitsWithBuckets(buckets, center), buckets), meetCondition)
    stream.head should equal((10, Stream((Location(10, 0), 10d))))
    stream.tail shouldBe empty
  }

  test("List((Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0))") {

    val location = Location(-27.059125784374057, -180.0)
    val temperatures = List((Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0))
    val temperatureBucketMap = MyTemperatureOrbit.classify(temperatures)
    MyTemperatureOrbit.findAtLeast10ClosestMeasures(location, temperatureBucketMap)
  }
}
