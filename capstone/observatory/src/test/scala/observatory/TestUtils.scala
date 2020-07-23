package observatory

import java.nio.file.{Files, Path}

object TestUtils {

  def writeToTempFile(content: String): Path = {

    val path: Path = Files.createTempFile("test", ".csv")
    Files.write(path, content.getBytes)
    path
  }
}
