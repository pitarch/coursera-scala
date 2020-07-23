package observatory

import org.apache.spark.SparkContext
import org.apache.spark.sql.SparkSession

trait SparkTestSupport {

  lazy val spark: SparkSession = SparkSession.builder().appName("extractor").master("local[*]").getOrCreate()
  lazy val sc: SparkContext = spark.sparkContext

}
