package observatory

import java.io.InputStream
import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    locateTemperaturesPar(year, stationsFile, temperaturesFile)
  }


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    locationYearlyAverageRecordsPar(records)
  }

  def readStationsFilePar(inputStream: InputStream): Stream[Station] = {
    def idToInt(s: String) = if (s.isEmpty) -1 else s.toInt

    Source.fromInputStream(inputStream, "utf-8")
      .getLines()
      .toStream
      .par
      .map(_.split(",", -1))
      .filter(a => !a(2).isEmpty && !a(3).isEmpty)
      // stn, wban, lat, lon
      .map(a => (idToInt(a(0)), idToInt(a(1)), a(2).toDouble, a(3).toDouble))
      //.filter { case (stn, wban, _, _) => stn != -1 && wban != -1 }
      .map { case (stn, wban, latitude, longitude) => Station(stn, wban, Location(latitude, longitude)) }
      .toStream
  }

  def readTemperaturePar(year: Int, file: InputStream): Stream[TemperatureMeasure] = {
    def idToInt(s: String) = if (s.isEmpty) -1 else s.toInt

    def farenheitToCelcius(t: Longitude) = (t - 32) * 5d / 9d

    Source.fromInputStream(file, "utf-8")
      .getLines()
      .toStream
      .par
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.split(",", -1))
      // stn, wban, month, day, temperature
      .map(a => (idToInt(a(0)), idToInt(a(1)), a(2).toInt, a(3).toInt, farenheitToCelcius(a(4).toDouble)))
      .map { case (stn, wban, month, day, temperature) => TemperatureMeasure(stn, wban, year, month, day, temperature) }
      .toStream
  }

  def locateTemperaturesPar(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationsInputStream = getClass.getResourceAsStream(stationsFile)
    val temperaturesInputStream = getClass.getResourceAsStream(temperaturesFile)
    locateTemperaturesPar(year, stationsInputStream, temperaturesInputStream)
  }


  def locateTemperaturesPar(year: Year, stationsInputStream: InputStream, temperaturesInputStream: InputStream): Iterable[(LocalDate, Location, Temperature)] = {
    val stations = this.readStationsFilePar(stationsInputStream)
      //.filter { station => station.stn != -1 && station.wban != -1 }
      .groupBy { s => (s.wban, s.stn) }
      .mapValues(_.head.location)

    this.readTemperaturePar(year, temperaturesInputStream)
      .filter { tm => stations.contains((tm.wban, tm.stn)) }
      .map { tm => (LocalDate.of(tm.year, tm.month, tm.day), stations((tm.wban, tm.stn)), tm.temperature) }
  }

  def locationYearlyAverageRecordsPar(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {

    records
      .par
      .map { case (_, location, temperature) => (location, temperature) }
      .groupBy { case (location, _) => location }
      .mapValues { iter =>
        val (total, length) = iter.map { case (_, temperature) => temperature }.foldLeft((0.0d, 0))((t, r) => (t._1 + r, t._2 + 1))
        total / length
      }
      .seq
  }
}
