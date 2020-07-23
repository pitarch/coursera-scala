package observatory

import com.sksamuel.scrimage.{Image, Pixel, RGBColor}
import org.slf4j.{Logger, LoggerFactory}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  val logger: Logger = LoggerFactory.getLogger(getClass)

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = Tile.toLocation(tile.x, tile.y, tile.zoom)

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    //    val msg = s"tile: $tile, temperatures: $temperatures, colors: $colors"
    //    throw new Error(msg)

    mine.doTile(temperatures, colors, tile)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {
    val stream = for {
      (year, data) <- yearlyData.toStream
      tile <- generateTileStream(Stream(Tile.TILE0)).takeWhile(_.zoom <= 3)
    } yield (year, data, tile)

    stream.foreach {
      case (year, data, tile) => generateImage(year, tile, data)
    }
  }


  def generateTileStream(stream: Stream[Tile]): Stream[Tile] = {
    if (stream.isEmpty) Stream.empty
    else {
      val tile = stream.head
      tile #:: generateTileStream(stream.tail ++ tile.zoomIn())
    }
  }

  object mine {

    def doTile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

      val tileSize = Tile.TILE_SIZE_IN_PIXELS
      val stream: Stream[(Int, Int)] = for {
        y <- (0 until tileSize).toStream
        x <- (0 until tileSize).toStream
      } yield (x, y)

      logger.debug(s"Generating a ${tileSize}x$tileSize and then we will resize it to scale factor=2")

      val skPixels: Array[Pixel] = stream
        .par
        .map { case (x, y) =>
          val location = tile.locationAtPixel(x, y)
          //        logger.info(s"resolving location $location...")
          if (x == 0) logger.info(s"tiling... resolving row $y location: $location")
          //        logger.debug(s"Using temperatures on location ${location}: $reducedTemperatures")
          val prediction = Visualization.predictTemperature(temperatures, location)
          val Color(red, green, blue) = Visualization.interpolateColor(colors, prediction)
          RGBColor(red, green, blue, 127).toPixel
        }.toArray

      Image(tileSize, tileSize, skPixels)
    }
  }

}
