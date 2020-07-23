package observatory

import java.io.{BufferedInputStream, InputStream}
import java.nio.file.{Files, Path}
import java.time.LocalDate

import observatory.TestUtils.writeToTempFile
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should

trait ExtractionTest extends MilestoneSuite with AnyFunSuiteLike with should.Matchers {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object

  // //////////////////////////////////////////////////////////////////////////
  // readStationFilePar
  // //////////////////////////////////////////////////////////////////////////

  test("readStationFilePar: should assign -1 when missing wban") {
    val content = "010010,,+70.933,-008.667"
    val inputStream: InputStream = writeAndLoadAsInputStream(content)
    val stations = Extraction.readStationsFilePar(inputStream)
    stations.head.stn shouldBe 10010
    stations.head.wban shouldBe -1
  }

  test("readStationFilePar: should assign -1 when missing stn") {
    val content = ",010010,+70.933,-008.667"
    val inputStream: InputStream = writeAndLoadAsInputStream(content)
    val stations = Extraction.readStationsFilePar(inputStream)
    stations.head.stn shouldBe -1
    stations.head.wban shouldBe 10010
  }

  test("readStationFilePar: should not omit station when both stn and wban are missing") {
    val content = ",,+70.933,-008.667"
    val inputStream: InputStream = writeAndLoadAsInputStream(content)
    val stations = Extraction.readStationsFilePar(inputStream)
    stations.head.stn shouldBe -1
    stations.head.wban shouldBe -1
  }

  test("readStationFilePar: should read stations") {
    val content = "010010,003,+70.933,-008.667"
    val inputStream: InputStream = writeAndLoadAsInputStream(content)
    val stations = Extraction.readStationsFilePar(inputStream)
    stations.head.stn shouldBe 10010
    stations.head.wban shouldBe 3
    stations.head.location shouldEqual Location(70.933, -8.667)
  }

  /*
  [Test Description] weather stations are identified by the composite (STN, WBAN) (3pts)(observatory.CapstoneSuite)
[Observed Error] expected:<5> but was:<2>


   */

  // //////////////////////////////////////////////////////////////////////////
  // readTempFilePar
  // //////////////////////////////////////////////////////////////////////////

  test("readTemperatureFilePar: should read a temperature file") {
    val content = "725335,94833,03,17,45.4"
    val inputStream: InputStream = writeAndLoadAsInputStream(content)
    val measures = Extraction.readTemperaturePar(2015, inputStream)
    val measure = measures.head
    measure.stn shouldBe 725335
    measure.wban shouldBe 94833
    measure.year shouldBe 2015
    measure.month shouldBe 3
    measure.day shouldBe 17
    measure.temperature shouldBe (7.44 +- 0.01)
  }

  test("readTemperatureFilePar: should assign -1 to wban when missing") {
    val content = "725335,,03,17,45.4"
    val inputStream: InputStream = writeAndLoadAsInputStream(content)
    val measures = Extraction.readTemperaturePar(2015, inputStream)
    val measure = measures.head
    measure.wban shouldBe -1
  }

  private def writeAndLoadAsInputStream(content: String): InputStream = {
    val path = writeToTempFile(content)
    val inputStream = new BufferedInputStream(Files.newInputStream(path))
    inputStream
  }

  test("readTemperatureFilePar: should assign -1 to stn when missing") {
    val content = ",94833,03,17,45.4"
    val inputStream: InputStream = writeAndLoadAsInputStream(content)
    val measures = Extraction.readTemperaturePar(2015, inputStream)
    val measure = measures.head
    measure.stn shouldBe -1
  }

  test("readTemperatureFilePar: should not omit station when both stn and wban are missing") {
    val content = ",,03,17,45.4"
    val inputStream: InputStream = writeAndLoadAsInputStream(content)
    val measures = Extraction.readTemperaturePar(2015, inputStream)
    val measure = measures.head
    measure.stn shouldBe -1
    measure.wban shouldBe -1
  }

  // //////////////////////////////////////////////////////////////////////////
  // locateTemperaturesPar
  // //////////////////////////////////////////////////////////////////////////

  test("locateTemperaturesPar: should assign temperatures to locations") {

    val isStations = writeAndLoadAsInputStream("010010,003,+70.933,-008.667")
    val isMeasures = writeAndLoadAsInputStream("010010,003,03,17,45.4")
    val iterableResult = Extraction.locateTemperaturesPar(2015, isStations, isMeasures)
    val (date, location, temperature) = iterableResult.head
    date shouldBe LocalDate.of(2015, 3, 17)
    temperature shouldBe (7.44 +- 0.01)
    location shouldBe Location(+70.933, -008.667)
  }


  // //////////////////////////////////////////////////////////////////////////
  // locationYearlyAverageRecordsPar
  // //////////////////////////////////////////////////////////////////////////

  test("locationYearlyAverageRecordsPar: should calculate the yearly average temperate by location 1") {

    val location = Location(+70.933, -008.667)
    val records = Stream(
      (LocalDate.of(2015, 3, 17), location, 7.44)
    )
    val resultIter: Iterable[(Location, Temperature)] = Extraction.locationYearlyAverageRecordsPar(records)
    val result: (Location, Temperature) = resultIter.head
    result._1 shouldEqual location
    result._2 shouldBe (7.44 +- 0.01)
  }

  test("locationYearlyAverageRecordsPar: should calculate the yearly average temperate by location 2") {

    val location = Location(+70.933, -008.667)
    val records = Stream(
      (LocalDate.of(2015, 3, 17), location, 7.44),
      (LocalDate.of(2015, 3, 18), location, 7.44)
    )
    val resultIter: Iterable[(Location, Temperature)] = Extraction.locationYearlyAverageRecordsPar(records)
    val result: (Location, Temperature) = resultIter.head
    result._1 shouldEqual location
    result._2 shouldBe (7.44 +- 0.01)
  }

  // ********************************************************
  // ********************************************************
  // ********************************************************


  test("should write a temporary file") {

    val path: Path = writeToTempFile("012")
    println(s"file: ${path}")
    new String(Files.readAllBytes(path)) shouldBe "012"
  }
}
