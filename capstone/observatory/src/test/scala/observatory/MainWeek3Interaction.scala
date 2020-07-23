package observatory

import java.time.LocalDate

import observatory.pittarck.MyVisualization
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

object MainWeek3Interaction {

  val logger: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {

    val tile = Tile.TILE0
    val year = 2015
    logger.info(s"Reading files...")
    val tempByLocationAndDate: Iterable[(LocalDate, Location, Temperature)] = Extraction.locateTemperaturesPar(year, "/stations.csv", s"/$year.csv")
    logger.info(s"calculating average temperatures...")
    //    val tempByLocation = Extraction.locationYearlyAverageRecordsPar(tempByLocationAndDate)
    val tempByLocation = Map(Location(45d, -90d) -> 10d, Location(-45d, 0) -> 20d)
    val image = Interaction.tile(tempByLocation, MyVisualization.constants.colors, tile)
    logger.info(s"output image...")
    val path = Try(image.output(s"target/temperatures/2015/${tile.zoom}/${tile.x}-${tile.y}.png"))
    path match {
      case Failure(exception) => logger.error(s"saving error: $exception")
      case Success(value) => logger.info(s"file save on $value")
    }
  }
}
