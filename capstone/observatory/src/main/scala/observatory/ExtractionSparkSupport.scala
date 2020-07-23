package observatory

import java.time.LocalDate

import org.apache.spark.sql.functions.{avg, lit, struct, udf}
import org.apache.spark.sql.types.{DoubleType, IntegerType, StructField, StructType}
import org.apache.spark.sql.{Dataset, SparkSession}

import scala.io.Source

object ExtractionSparkSupport {

  val stationSchema: StructType = StructType(
    Seq(
      StructField("stn", IntegerType, nullable = true),
      StructField("wban", IntegerType, nullable = true),
      StructField("lat", DoubleType, nullable = false),
      StructField("lon", DoubleType, nullable = false)
    )
  )

  val temperatureFileSchema: StructType = StructType(
    Seq(
      StructField("stn", IntegerType, nullable = true),
      StructField("wban", IntegerType, nullable = true),
      StructField("month", IntegerType, nullable = false),
      StructField("day", IntegerType, nullable = false),
      StructField("temperature", DoubleType, nullable = false)
    )
  )


  def locationYearlyAverageRecordsSpark(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    import observatory.SparkSupport.spark
    import org.apache.log4j.{Level, Logger}
    import spark.implicits._
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

    import scala.collection.JavaConverters._

    val df = records
      .toStream
      .map { case (date, location, temperature) => (date.getYear, location, temperature) }
      .toDF("year", "location", "temperature")

    val resultDs = df
      .groupBy($"location")
      .agg(avg("temperature").as("avg"))
      .select($"location", $"avg".as("temperature"))
      .as[(Location, Temperature)]

    val iterResult = resultDs
      .toLocalIterator().asScala.toIterable

    iterResult
  }

  def readStationsFileSpark(spark: SparkSession, stationsFile: String): Dataset[Station] = {
    val file = getClass.getResourceAsStream(stationsFile)
    import spark.implicits._

    def idToInt(s: String) = if (s.isEmpty) -1 else s.toInt

    val inputSeq = Source.fromInputStream(file, "utf-8")
      .getLines()
      .map(_.split(",", -1))
      .filter(a => !a(2).isEmpty && !a(3).isEmpty)
      .map(a => (idToInt(a(0)), idToInt(a(1)), a(2).toDouble, a(3).toDouble))
      .toSeq

    val ds = spark.sparkContext.parallelize(inputSeq)
      .toDF("stn", "wban", "lat", "lon")
      .withColumn("location", struct($"lat", $"lon"))
      .drop("lat", "lon")
      .select('stn, 'wban, 'location)
      .as[Station]

    ds.repartition($"stn", $"wban")
  }

  def readTemperatureFileSpark(spark: SparkSession, year: Int, temperaturesFile: String): Dataset[TemperatureMeasure] = {
    val file = getClass.getResourceAsStream(temperaturesFile)

    def idToInt(s: String) = if (s.isEmpty) -1 else s.toInt

    val inputSeq = Source.fromInputStream(file, "utf-8")
      .getLines()
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.split(",", -1))
      .map(a => (idToInt(a(0)), idToInt(a(1)), a(2).toInt, a(3).toInt, a(4).toDouble))
      .toSeq

    import spark.implicits._
    val toCelsiusUdf = udf((t: Double) => (t - 32) * 5d / 9d)
    val ds = spark.sparkContext
      .parallelize(inputSeq)
      .toDF("stn", "wban", "month", "day", "temperature")
      .withColumn("year", lit(year))
      .withColumn("temperature", toCelsiusUdf($"temperature"))
      .select($"stn", $"wban", $"year", $"month", $"day", $"temperature")
      .as[TemperatureMeasure]
    ds
  }

  //  def locateTemperaturesSpark(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
  //    import SparkSupport.spark
  //    import spark.implicits._
  //
  //    import collection.JavaConverters._
  //
  //    val stationDs: Dataset[Station] = this.readStationsFile(spark, stationsFile)
  //    val tempMesureDs: Dataset[TemperatureMeasure] = this.readTemperatureFilePar(spark, year, temperaturesFile)
  //    val ds = stationDs.joinWith(tempMesureDs, stationDs("stn") === tempMesureDs("stn") && stationDs("wban") === tempMesureDs("wban"), "inner")
  //      .map { case (station, measure) => (measure.year, measure.month, measure.day, station.location, measure.temperature) }
  //      .toLocalIterator()
  //      .asScala
  //      .toStream
  //      .map { case (year, month, day, location, temperature) => (LocalDate.of(year, month, day), location, temperature) }
  //
  //    ds
  //  }
}
