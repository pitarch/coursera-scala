package observatory

import com.sksamuel.scrimage.{Image, RGBColor}
import observatory.pittarck.MyVisualization

import scala.util.Try

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    MyVisualization.prediction.predictTemperature(temperatures, location)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
    MyVisualization.interpolation.interpolateColor(points, value)


  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    Try {
      visualizeMine(temperatures, colors)
    }.toEither match {
      case Left(error) =>
        val sTemps = temperatures.take(20).map { case (location, temperature) => s"$location:$temperature" }.reduce(_ + ", " + _)
        val sColors = colors.map { case (temperature, color) => s"$temperature:$color" }.reduce(_ + ", " + _)
        throw new Error(s"$error\n\ttemperatures: $sTemps\n\tcolors: $sColors")
      case Right(image) =>
        image
    }
  }


  def visualizeMine(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val earthCoords = for {
      latitude <- Stream.range(90, -91, -1)
      longitude <- -180 to 180
    } yield (latitude, longitude)

    val pixels = earthCoords
      .par
      .map { case (latitude, longitude) =>
        val location = Location(latitude, longitude)
        val prediction = predictTemperature(temperatures, location)
        val color = interpolateColor(colors, prediction)
        val pixel = RGBColor(color.red, color.green, color.blue).toPixel
        pixel
      }.toArray

    Image(361, 181, pixels)
  }
}

