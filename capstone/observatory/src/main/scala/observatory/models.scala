package observatory

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double)

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  *
  * @param x    X coordinate of the tile
  * @param y    Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int) {

  lazy val location: Location = Tile.toLocation(x, y, zoom)

  def locationAtPixel(pixelX: Int, pixelY: Int): Location =
    Tile.toLocation(x * 256 + pixelX, y * 256 + pixelY, zoom + 8)

  def locationToTilePosition(location: Location): (Int, Int) = Tile.locationToTilePosition(location, zoom)

  def locationToPixelPosition(location: Location): (Int, Int) = Tile.locationToTilePosition(location, zoom + 8)

  def zoomIn(): Stream[Tile] = zoomInNw #:: zoomInNe #:: zoomInSw #:: zoomInSe #:: Stream.empty

  lazy val zoomInNw: Tile = Tile(2 * x, 2 * y, zoom + 1)

  lazy val zoomInNe: Tile = Tile(2 * x + 1, 2 * y, zoom + 1)

  lazy val zoomInSw: Tile = Tile(2 * x, 2 * y + 1, zoom + 1)

  lazy val zoomInSe: Tile = Tile(2 * x + 1, 2 * y + 1, zoom + 1)
}

object Tile {

  import Math._

  val TILE_SIZE_IN_PIXELS = 256
  val TILE0: Tile = Tile(0, 0, 0)

  lazy val MAX_LATITUDE: Latitude = toDegrees(atan(sinh(PI)))
  lazy val MIN_LATITUDE: Latitude = -MAX_LATITUDE

  def toLocation(tileX: Int, tileY: Int, zoom: Int): Location = {
    val n = 1 << zoom
    val lat = toDegrees(atan(sinh(PI * (1.0 - 2.0 * tileY.toDouble / n))))
    val lon = tileX.toDouble / n * 360.0 - 180.0
    Location(lat = lat, lon = lon)
  }


  def locationToTilePosition(location: Location, zoom: Int): (Int, Int) = {
    val n = 1 << zoom
    var xtile = Math.floor((location.lon + 180) / 360 * n).toInt
    var ytile = Math.floor((1 - Math.log(Math.tan(Math.toRadians(location.lat)) + 1 / Math.cos(Math.toRadians(location.lat))) / Math.PI) / 2 * n).toInt
    if (xtile < 0) xtile = 0
    if (xtile >= n) xtile = n - 1
    if (ytile < 0) ytile = 0
    if (ytile >= n) ytile = n - 1
    (xtile, ytile)
  }
}

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  *
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int)

object GridLocation {

  def locationStream: Stream[GridLocation] = {
    val result: Stream[GridLocation] = for {
      y <- (90 to -89 by -1).toStream
      x <- -180 to 179
    } yield GridLocation(x, y)
    result
  }
}

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  *
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  *
  * @param red   Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue  Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int)
