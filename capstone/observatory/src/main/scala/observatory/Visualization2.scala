package observatory

import com.sksamuel.scrimage.Image

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00   Top-left value
    * @param d01   Bottom-left value
    * @param d10   Top-right value
    * @param d11   Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             point: CellPoint,
                             d00: Temperature,
                             d01: Temperature,
                             d10: Temperature,
                             d11: Temperature
                           ): Temperature = {
    val x = point.x
    val y = point.y
    val result = d00 * (1d - x) * (1d - y) + d10 * x * (1d - y) + d01 * (1d - x) * y + d11 * x * y
    result
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param tile   Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: GridLocation => Temperature,
                     colors: Iterable[(Temperature, Color)],
                     tile: Tile
                   ): Image = {

    //    val temperatures = for {
    //      y <- -89 to 90
    //      x <- -180 to 179
    //      gridLoc = GridLocation(x, y)
    //    } yield (Location(x, y), grid(gridLoc))
    //    throw new Error(s"tile: $tile\n\ttemperatures: $temperatures\n\tcolors: $colors")


    val temperatures: Iterable[(Location, Temperature)] = GridLocation.locationStream
      .map { case loc@GridLocation(lat, lon) => (Location(lat, lon), grid(loc)) }
    val image = Interaction.tile(temperatures, colors, tile)
    image
  }

}
