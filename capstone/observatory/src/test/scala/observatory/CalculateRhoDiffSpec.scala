package observatory

import observatory.pittarck.MyVisualization
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpecLike
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CalculateRhoDiffSpec extends AnyPropSpecLike with ScalaCheckPropertyChecks with should.Matchers {

  val latitudeGenerator: Gen[Longitude] = Gen.chooseNum(-90d, 90d)
  val longitudeGenerator: Gen[Longitude] = Gen.chooseNum(-180d, 180d)
  val locationGenerator: Gen[Location] = for {
    lat <- latitudeGenerator
    lon <- longitudeGenerator
  } yield Location(lat, lon)

  implicit lazy val locationArb: Arbitrary[Location] = Arbitrary(locationGenerator)

  property("rhoDiff") {
    forAll { (loc1: Location, loc2: Location) =>
      MyVisualization.prediction.calculateRhoDiff(loc1, loc2) should (be >= 0d and be <= Math.PI)
    }
  }
}
