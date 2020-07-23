package observatory.pittarck

import observatory.{Location, Temperature}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ListBuffer

object MyTemperatureOrbit {

  type Point = (Int, Int)
  type OrbitCenter = (Int, Int)
  type LocationTemperaturePair = (Location, Temperature)
  type BucketMap = Map[OrbitCenter, ListBuffer[LocationTemperaturePair]]
  type OrbitStream = Stream[Point]

  def classify(temperatures: Iterable[LocationTemperaturePair]): Map[OrbitCenter, ListBuffer[LocationTemperaturePair]] = {

    val buckets: TrieMap[(Int, Int), ListBuffer[(Location, Temperature)]] = TrieMap[OrbitCenter, ListBuffer[LocationTemperaturePair]]()

    temperatures.foreach { case locTempPair@(location, _) =>
      val orbitCenter = (location.lat.toInt, location.lon.toInt)
      if (!buckets.contains(orbitCenter)) buckets.put(orbitCenter, ListBuffer[LocationTemperaturePair]())
      buckets(orbitCenter).append(locTempPair)
    }
    buckets.toMap
  }

  def generateTopOrbits(centerX: Int, centerY: Int, radio: Int): Stream[Point] = {
    Stream.range(centerY - radio, centerY + radio + 1).map((centerX - radio, _))
  }

  def generateBottomOrbits(centerX: Int, centerY: Int, radio: Int): Stream[Point] = {
    Stream.range(centerY - radio, centerY + radio + 1).map((centerX + radio, _))
  }

  def generateLateralOrbits(centerX: Int, centerY: Int, radio: Int): Stream[Point] = {
    Stream.range(centerX - radio + 1, centerX + radio).flatMap { x => Seq((x, centerY - radio), (x, centerY + radio)) }
  }

  def generateOrbitWithRadio(center: OrbitCenter, radio: Int = 1): Stream[Point] = {
    val (x, y) = center
    generateTopOrbits(x, y, radio) ++ generateLateralOrbits(x, y, radio) ++ generateBottomOrbits(x, y, radio)
  }

  def isWithin(point: Point): Boolean = {
    val (x, y) = point
    (y >= -180 && y <= 180) && (x >= -90 && x <= 90)
  }

  def toMeasures(point: Point, buckets: BucketMap): Iterable[LocationTemperaturePair] =
    if (buckets.contains(point)) buckets(point)
    else Iterable.empty

  def havingMeasures(point: Point, buckets: Map[OrbitCenter, ListBuffer[LocationTemperaturePair]]): Boolean =
    buckets.contains(point)

  def streamOrbits(center: OrbitCenter, radio: Int = 1): Stream[(Int, Stream[Point])] = {
    val shouldDig = radio <= 360
    if (shouldDig)
      (radio, generateOrbitWithRadio(center, radio).filter(isWithin)) #:: streamOrbits(center, radio + 1)
    else Stream.empty
  }

  def streamOrbitsWithBuckets(buckets: BucketMap, center: OrbitCenter, radio: Int = 1): Stream[(Int, Stream[Point])] =
    streamOrbits(center, radio)
      // remove points without temperature measures in each orbit
      .map { case (radio, stream) => (radio, stream.filter(havingMeasures(_, buckets))) }
      // remove orbits with no temperature measures
      .filterNot { case (_, points) => points.isEmpty }

  def toTemperatureMeasuresByOrbit(stream: Stream[(Int, Stream[Point])], buckets: BucketMap): Stream[(Int, Iterable[LocationTemperaturePair])] =
    stream.map { case (radio, points) => (radio, points flatMap buckets) }

  def enoughTemperatureForPrediction(prevRadio: Int, radio: Int, count: Int): Boolean = count > 10 || radio >= 180

  def streamUntilCondition(stream: Stream[(Int, Iterable[LocationTemperaturePair])],
                           meetCondition: (Int, Int, Int) => Boolean = enoughTemperatureForPrediction,
                           prevRadio: Int = 0,
                           count: Int = 0): Stream[(Int, Iterable[LocationTemperaturePair])] = {
    stream match {
      case Stream.Empty => Stream.empty
      case h@(radio, temperatures) #:: tail =>
        if (meetCondition(prevRadio, radio, count)) Stream.empty
        else (radio, temperatures) #:: streamUntilCondition(tail, meetCondition, radio, count + temperatures.size)
    }
  }

  def findAtLeast10ClosestMeasures(location: Location, buckets: BucketMap): Iterable[(Location, Temperature)] = {
//    logger.debug(s"Finding closest measures on location ${location}...")
    val meetCondition = (prevRadio: Int, radio: Int, count: Int) => count > 10
    val center = (location.lat.toInt, location.lon.toInt)
    val positionStreamByOrbit = streamOrbitsWithBuckets(buckets, center)
    val measureStreamByOrbit = toTemperatureMeasuresByOrbit(positionStreamByOrbit, buckets)
    val stream = streamUntilCondition(measureStreamByOrbit, meetCondition)
    stream.flatMap(_._2).toList
  }
}

