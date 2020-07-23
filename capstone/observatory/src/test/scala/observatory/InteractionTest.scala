package observatory

import com.sksamuel.scrimage.RGBColor
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should

import scala.collection.mutable.ListBuffer

class InteractionTest extends /*MilestoneSuite with*/ AnyFunSuiteLike with should.Matchers {

  //  private val milestoneTest = namedMilestoneTest("data visualization", 3) _

  test("toLocation") {
    Interaction.tileLocation(Tile.TILE0) shouldBe Location(Tile.MAX_LATITUDE, -180)
  }

  test("locationAtPixel at zoom 0") {
    val tile = Tile.TILE0
    tile.locationAtPixel(0, 0) shouldBe Location(Tile.MAX_LATITUDE, -180)
    tile.locationAtPixel(255, 0) shouldBe Location(Tile.MAX_LATITUDE, 178.59375)
    tile.locationAtPixel(0, 255) shouldBe Location(-84.92832092949963, -180)
    tile.locationAtPixel(255, 255) shouldBe Location(-84.92832092949963, 178.59375)
    tile.locationAtPixel(128, 128) shouldBe Location(0, 0)
  }

  test("locationAtPixel at zoom 1") {
    val tile = Tile.TILE0
    tile.zoomInSe.locationAtPixel(0, 0) shouldBe Location(0, 0)
    tile.zoomInSe.locationAtPixel(255, 255) shouldBe Location(-84.99010018023479, 179.296875)
  }

  test("locationToPixelPosition") {
    val tile = Tile.TILE0

    tile.locationToPixelPosition(Location(90, -180)) shouldBe ((0, 0))
    tile.locationToPixelPosition(Location(90, 180)) shouldBe ((255, 0))
    tile.locationToPixelPosition(Location(-90, -180)) shouldBe ((0, 255))
    tile.locationToPixelPosition(Location(-90, 180)) shouldBe ((255, 255))
    tile.locationToPixelPosition(Location(0, 0)) shouldBe ((128, 128))
  }

  test("pixel size is double at zoom 0 regarding the pixel size at zoom 1") {

    val tile = Tile.TILE0
    val loc00 = tile.locationAtPixel(0, 0)
    val loc10 = tile.locationAtPixel(1, 1)
    val (incrLat0, incrLon0) = (loc00.lat - loc10.lat, loc10.lon - loc00.lon)
    val loc01 = tile.zoomInNw.locationAtPixel(0, 0)
    val loc11 = tile.zoomInNw.locationAtPixel(1, 1)
    val (incrLat1, incrLon1) = (loc01.lat - loc11.lat, loc11.lon - loc01.lon)
    incrLat0 === (incrLat1 * 2) +- 1e-5
    incrLon0 === (incrLon1 * 2) +- 1e-5
  }


  // ///////////////////////////////////////////////////////
  // locationToTilePosition
  // ///////////////////////////////////////////////////////
  test("locationToTilePosition at zoom 0") {
    val tile = Tile.TILE0
    tile.locationToTilePosition(Location(Tile.MAX_LATITUDE, -180)) shouldBe ((0, 0))
    tile.locationToTilePosition(Location(Tile.MAX_LATITUDE, 180)) shouldBe ((0, 0))
    tile.locationToTilePosition(Location(-Tile.MAX_LATITUDE, 180)) shouldBe ((0, 0))
    tile.locationToTilePosition(Location(-Tile.MAX_LATITUDE, -180)) shouldBe ((0, 0))
    tile.locationToTilePosition(Location(Tile.MAX_LATITUDE, 0)) shouldBe ((0, 0))
    tile.locationToTilePosition(Location(-Tile.MAX_LATITUDE, 0)) shouldBe ((0, 0))
    tile.locationToTilePosition(Location(0, 180)) shouldBe ((0, 0))
    tile.locationToTilePosition(Location(0, -180)) shouldBe ((0, 0))
    tile.locationToTilePosition(Location(0, 0)) shouldBe ((0, 0))
  }

  test("locationToTilePosition at zoom 1") {
    val tile = Tile.TILE0
    tile.zoomInNw.locationToTilePosition(Location(45, -90)) shouldBe ((0, 0))
    tile.zoomInNe.locationToTilePosition(Location(45, 90)) shouldBe ((1, 0))
    tile.zoomInSw.locationToTilePosition(Location(-45, -90)) shouldBe ((0, 1))
    tile.zoomInSe.locationToTilePosition(Location(-45, 90)) shouldBe ((1, 1))
  }

  test("tile must be consistent across zoom levels (3pts)") {

    val temperatures = Seq(
      (Location(45.0, -90.0), 20.0),
      (Location(45.0, 90.0), 0.0),
      (Location(0.0, 0.0), 10.0),
      (Location(-45.0, -90.0), 0.0),
      (Location(-45.0, 90.0), 20.0))
    val colors = List(
      (0.0, Color(255, 0, 0)),
      (10.0, Color(0, 255, 0)),
      (20.0, Color(0, 0, 255)))

    //    val image = MyInteraction.tile(temperatures, colors, Tile.TILE0)
    //    val imageNw = MyInteraction.tile(temperatures, colors, Tile.TILE0.zoomInNw)
    //    val imageNe = MyInteraction.tile(temperatures, colors, Tile.TILE0.zoomInNe)
    //    val imageSw = MyInteraction.tile(temperatures, colors, Tile.TILE0.zoomInSw)
    //    val imageSe = MyInteraction.tile(temperatures, colors, Tile.TILE0.zoomInSe)
    //    imageNw.pixel(0, 0).toColor shouldBe image.pixel(0, 0).toColor
    //    imageNw.pixel(2, 2).toColor shouldBe image.pixel(1, 1).toColor
    //    imageNw.pixel(255, 255).toColor shouldBe imageSe.pixel(0, 0).toColor
    //    imageSe.pixel(0, 0).toColor shouldBe image.pixel(128, 128).toColor
  }


  // ///////////////////////////////////////////////////////
  // generateTiles
  // ///////////////////////////////////////////////////////

  test("generateTiles") {

    val yearlyData = List((1900, ()))
    val images = ListBuffer[(Year, Tile)]()

    def imageGenerator(year: Year, tile: Tile, data: Unit): Unit = {
      images.append((year, tile))
    }

    Interaction.generateTiles[Unit](yearlyData, imageGenerator)
    images should have length 85
    images.filter { case (_, tile) => tile.zoom == 0 } should have length 1
    images.filter { case (_, tile) => tile.zoom == 1 } should have length 4
    images.filter { case (_, tile) => tile.zoom == 2 } should have length 16
    images.filter { case (_, tile) => tile.zoom == 3 } should have length 64
  }


  test("tile pixel colors must be consistent with the given located temperatures and color scale (5pts)") {

    // [Test Description] tile pixel colors must be consistent with the given located temperatures and color scale (5pts)(observatory.CapstoneSuite)
    //
    // temperatures: List((Location(45.0,-90.0),10.0), (Location(-45.0,0.0),20.0))
    // colors: List((10.0,Color(255,0,0)), (20.0,Color(0,0,255)))
    // tile: Tile(0,0,0)
    //
    // Incorrect computed color at Location(-27.059125784374057,-180.0): Color(197,0,58).
    // Expected to be closer to Color(0,0,255) than Color(255,0,0)

    val temperatures = List((Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0))
    val colors = List((10.0, Color(255, 0, 0)), (20.0, Color(0, 0, 255)))
    val tile = Tile(0, 0, 0)
    val targetLocation = Location(-27.05912, -180.0)
    val predictedTempOnTarget = Visualization.predictTemperature(temperatures, targetLocation)
    val expectedColor = RGBColor(0, 0, 255, 127) // or close
    val image = Interaction.tile(temperatures, colors, tile)
    val (px, py) = tile.locationToPixelPosition(targetLocation)
    val actualColor = image.pixel(px, py).toColor
    actualColor.red should be < actualColor.blue
    1 should be > 0
  }


  test("tile pixel colors must be consistent with the given located temperatures and color scale") {

    val tile = Tile(0, 0, 0)
    val temperatures = List((Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0))
    val colors = List((10.0, Color(255, 0, 0)), (20.0, Color(0, 0, 255)))

    // Expected to be closer to Color(255,0,0) than Color(0,0,255)
    val image = Interaction.tile(temperatures, colors, tile)
    val targetLocation = Location(85.05112877980659,-180.0)
    val targetPixelPosition = tile.locationToPixelPosition(targetLocation)
    val auctualPixel = image.pixel(targetPixelPosition._1, targetPixelPosition._2).toColor
    auctualPixel.red > auctualPixel.blue
  }
}
