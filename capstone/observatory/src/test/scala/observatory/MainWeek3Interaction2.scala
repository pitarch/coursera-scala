package observatory

import observatory.pittarck.MyVisualization
import org.slf4j.{Logger, LoggerFactory}

object MainWeek3Interaction2 {

  val logger: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {

    val tile = Tile.TILE0
    val temperatures = List((Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0))
//    val colors = List((10.0, Color(255, 0, 0)), (20.0, Color(0, 0, 255)))
    val colors1 = MyVisualization.constants.colors
    //    val targetLocation = Location(-27.05912, -180.0)
    //    val predictedTempOnTarget = Visualization.predictTemperature(temperatures, targetLocation)
    //    val expectedColor = RGBColor(0, 0, 255, 127) // or close
    val image = Interaction.tile(temperatures, colors1, tile)
    //    val (px, py) = tile.locationToPixelPosition(targetLocation)
    //    val actualColor = image.pixel(px, py).toColor
    println(s"${image.pixel(1,1)}")
  }
}
