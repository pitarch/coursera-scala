package timeusage

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types.{DoubleType, StringType, StructField, StructType}
import org.junit.Test
import org.scalatest.matchers.should

class TimeUsageSuite extends should.Matchers {

  import TimeUsage._

  @Test
  def `timeUsageSummary should generate the right schema`: Unit = {

    val (columns, initDf) = read("../atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf.sample(0.01))
    val expectedSchema = StructType(Seq(
      StructField("working", StringType, false),
      StructField("sex", StringType, false),
      StructField("age", StringType, false),
      StructField("primaryNeeds", DoubleType, true),
      StructField("work", DoubleType, true),
      StructField("other", DoubleType, true)))

    summaryDf.printSchema()
    summaryDf.show(10)
    summaryDf.schema shouldBe expectedSchema
  }

  @Test
  def `timeUsageGrouped should group by working status, sex and age`: Unit = {
    import TimeUsage.spark.implicits._

    val (columns, initDf) = read("../atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf.sample(0.01))
    val avgDf = summaryDf
      .groupBy($"working", $"sex", $"age")
      .agg(
        round(avg($"primaryNeeds"), 1).as("primaryNeeds"),
        round(avg($"work"), 1).as("work"),
        round(avg($"other"), 1).as("other")
      )
      .sort("working", "sex", "age")
    avgDf.printSchema()
    avgDf.show(10)
  }

  @Test
  def `timeUsageGroupedSql should group by working status, sex and age`: Unit = {

    val (columns, initDf) = read("../atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf.sample(0.01))
    val avgDf = TimeUsage.timeUsageGroupedSql(summaryDf)
    avgDf.printSchema()
    avgDf.show(10)
  }


  @Test
  def `timeUsageGroupedTyped vs timeUsageGroupedSql`: Unit = {

    Logger.getLogger("org").setLevel(Level.OFF)
    val (columns, initDf) = read("../atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf.sample(0.01)).cache()
    val avgDf = TimeUsage.timeUsageGrouped(summaryDf)
    val avgSqlDf = TimeUsage.timeUsageGroupedSql(summaryDf)
    val avgTypedDf = TimeUsage.timeUsageGroupedTyped(TimeUsage.timeUsageSummaryTyped(summaryDf))

    avgDf.show(10)
    avgSqlDf.show(10)
    avgTypedDf.show(10)

  }
}
