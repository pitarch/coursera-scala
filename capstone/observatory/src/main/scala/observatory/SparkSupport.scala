package observatory

import org.apache.spark.sql.SparkSession

object SparkSupport {

  lazy val spark: SparkSession = SparkSession
    .builder()
    .appName("observatory")
    .master("local[*]")
    .getOrCreate()

//  spark.sparkContext.setLogLevel("ERROR")
}
