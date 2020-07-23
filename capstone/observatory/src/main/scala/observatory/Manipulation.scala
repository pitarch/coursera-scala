package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val map = scala.collection.mutable.HashMap[GridLocation, Temperature]()
    (gridLoc: GridLocation) => map.getOrElseUpdate(gridLoc, Visualization.predictTemperature(temperatures, Location(gridLoc.lat, gridLoc.lon)))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val gridFuncByYears: Iterable[GridLocation => Temperature] = temperaturess map makeGrid
    val result = (gridLoc: GridLocation) => {
      val (sum, size) = gridFuncByYears
        .map(f => f(gridLoc))
        .foldLeft((0d, 0))({ case ((s, l), x) => (s + x, l + 1) })
      sum / size
    }
    result
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val fGrid = makeGrid(temperatures)
    val deviationCache = scala.collection.mutable.HashMap[GridLocation, Temperature]()
    (gridLoc: GridLocation) => deviationCache.getOrElseUpdate(gridLoc, fGrid(gridLoc) - normals(gridLoc))
  }
}

