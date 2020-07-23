package observatory.pittarck

import observatory.{Color, Location, Temperature}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

object MyVisualization {

  case class TemperatureColor(temperature: Temperature, color: Color)

  object constants {

    def equatorialRadius: Double = 6378.137 // km
    def polesRadius: Double = 6356.752 // km
    def meanEarthRadius: Double = (2d * equatorialRadius + polesRadius) / 3d

    def antipodesDistance: Double = Math.PI * meanEarthRadius

    lazy val colors = Seq(
      (60d, Color(255, 255, 255)),
      (32d, Color(255, 0, 0)),
      (12d, Color(255, 255, 0)),
      (0d, Color(0, 255, 255)),
      (-15d, Color(0, 0, 255)),
      (-27d, Color(255, 0, 255)),
      (-50d, Color(33, 0, 107)),
      (-60d, Color(0, 0, 0))
    )
  }

  object prediction {

    val logger: Logger = LoggerFactory.getLogger(getClass)

    import constants._

    def predictTemperature2(
                             temperatures: Iterable[(Location, Temperature)],
                             targetLocation: Location,
                             weightingFunction: Double => Double = calculateShepardWeight(2))

    : Temperature = {

      def areSamePoint(loc1: Location, loc2: Location, epsilon: Double = 1e-5): Boolean =
        (loc1.lat - loc2.lat).abs + (loc1.lon - loc2.lon).abs <= epsilon

      def areAntipode(loc1: Location, loc2: Location): Boolean =
        areSamePoint(loc1, loc2.copy(lat = -loc2.lat)) || areSamePoint(loc1, loc2.copy(lon = -loc2.lon))

      def calculateRho(loc1: Location, loc2: Location): Double = {
        if (areSamePoint(loc1, loc2)) 0d
        else if (areAntipode(loc1, loc2)) Math.PI
        else {
          val rad1Lat = loc1.lat.toRadians
          val rad1Lon = loc1.lon.toRadians
          val rad2Lat = loc2.lat.toRadians
          val rad2Lon = loc2.lon.toRadians
          val rho = Math.acos(Math.sin(rad1Lat) * Math.sin(rad2Lat) + Math.cos(rad1Lat) * Math.cos(rad2Lat) * Math.cos(rad1Lon - rad2Lon))
          rho
        }
      }

      def calculateGreatCircleDistance(loc1: Location, loc2: Location): Double = {
        val rho = calculateRho(loc1, loc2)
        val d = meanEarthRadius * rho
        if (d <= 1d || d.isNaN) 0d
        else d
      }

      val (near, far) = temperatures
        .map { case (location, temperature) => (temperature, calculateGreatCircleDistance(location, targetLocation)) }
        .partition { case (_, d) => d <= 1d }

      if (near.nonEmpty) near.head._1
      else {
        val weightedTemperatures = far.map { case (temperature, d) => (temperature, weightingFunction(d)) }
        val totalWeight = weightedTemperatures.map(_._2).sum
        val dividend = weightedTemperatures.map { case (temperature, w) => w * temperature }.sum
        dividend / totalWeight
      }
    }


    def predictTemperature(
                            temperatures: Iterable[(Location, Temperature)],
                            targetLocation: Location,
                            weightingFunction: Double => Double = calculateShepardWeight(2))
    : Temperature = {

      val distTempPartitions = temperatures
        .map { case (location, temperature) => (calculateGreatCircleDistance(location, targetLocation), temperature) }
        .partition { case (distance, _) => distance <= 1d }

      val nearDistTempPairs = distTempPartitions._1
      val farDistTempPairs = distTempPartitions._2

      val closestDistTempPair = nearDistTempPairs.headOption
      if (closestDistTempPair.isDefined) {
        closestDistTempPair.get._2
      } else {
        val (dividend, divisor) = farDistTempPairs
          .map { case (distance, temperature) =>
            val weight = weightingFunction(distance)
            val dividend = weight * temperature
            (dividend, weight)
          }
          .reduce((left, right) => (left._1 + right._1, left._2 + right._2))
        val result = dividend / divisor
        //        logger.debug(s"predicate temperature on location $targetLocation: $result")
        result
      }
    }

    def calculateShepardWeight(powerParameter: Int = 2)(distance: Double): Double = 1d / Math.pow(distance, powerParameter)

    def calculateRhoDiff(loc1: Location, loc2: Location): Double = {
      val rad1Lat = loc1.lat.toRadians
      val rad1Lon = loc1.lon.toRadians
      val rad2Lat = loc2.lat.toRadians
      val rad2Lon = loc2.lon.toRadians
      val rhoDiff = Math.acos(Math.sin(rad1Lat) * Math.sin(rad2Lat) + Math.cos(rad1Lat) * Math.cos(rad2Lat) * Math.cos(rad1Lon - rad2Lon))
      rhoDiff
    }

    def calculateGreatCircleDistance(loc1: Location, loc2: Location): Double = {
      val rhoDiff: Double = calculateRhoDiff(loc1, loc2)
      val d = meanEarthRadius * rhoDiff

      if (d <= 1d || d.isNaN) 0d
      else d
    }

  }

  object interpolation {

    val epsilonTemperature = 1e-4

    def areSimilarTemperatures(a: Double, b: Double): Boolean = (a - b).abs <= epsilonTemperature

    def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

      val optLower: Option[(Temperature, Color)] = findByFilter(value, points, _._1 <= value)
      val optUpper: Option[(Temperature, Color)] = findByFilter(value, points, _._1 >= value)

      val result = (optLower, optUpper) match {
        case (None, Some((_, upperColor))) => upperColor
        case (Some((_, lowerColor)), None) => lowerColor
        case (Some((lowerTemp, lowerColor)), Some((upperTemp, upperColor))) =>
          if (areSimilarTemperatures(lowerTemp, value)) lowerColor
          else if (areSimilarTemperatures(upperTemp, value)) upperColor
          else interpolateLinearly2(value, lowerTemp, lowerColor, upperTemp, upperColor)
        case (None, None) =>
          throw new IllegalStateException(s"Unable to find the color category for temperature $value on scales $points")
      }
      result
    }


    private def interpolateLinearly2(targetTemperature: Temperature, lowerTemp: Temperature, lowerColor: Color, upperTemp: Temperature, upperColor: Color) = {

      def calculateWeight(x: Double, x0: Double, x1: Double) = (x - x0) / (x1 - x0)

      def calculateColorParam(y0: Double, y1: Double, weight: Double) = y0 * (1 - weight) + y1 * weight

      def normalizeColorParam(p: Double) = Math.min(255, p.round.toInt)

      val weight = calculateWeight(targetTemperature, lowerTemp, upperTemp)
      val red = normalizeColorParam(calculateColorParam(lowerColor.red, upperColor.red, weight))
      val green = normalizeColorParam(calculateColorParam(lowerColor.green, upperColor.green, weight))
      val blue = normalizeColorParam(calculateColorParam(lowerColor.blue, upperColor.blue, weight))

      Color(red, green, blue)
    }

    def calculateColorParam(upperParam: Int, lowerParam: Int, upperWeight: Double, lowerWeight: Double): Int =
      (upperParam * upperWeight + lowerParam * lowerWeight).round.toInt

    def findByFilter(target: Temperature, points: Iterable[(Temperature, Color)], filter: ((Temperature, Color)) => Boolean): Option[(Temperature, Color)] = {
      Try {
        points
          .filter(filter)
          .map { case (temperature, color) => ((target - temperature).abs, temperature, color) }
          .minBy(_._1)
      }
        .toOption
        .map { case (_, temperature, color) => (temperature, color) }
    }
  }

}
