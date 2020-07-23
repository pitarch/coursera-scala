package observatory

object Main extends App {

  import org.apache.log4j.{Level, Logger}

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

}
