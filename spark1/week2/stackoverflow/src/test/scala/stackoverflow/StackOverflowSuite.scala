package stackoverflow

import org.apache.spark.{SparkConf, SparkContext}
import org.junit._
import stackoverflow.StackOverflow.{groupedPostings, kmeans, rawPostings, sampleVectors, scoredPostings, vectorPostings}
object StackOverflowSuite {
  val conf: SparkConf = new SparkConf().setMaster("local[*]").setAppName("StackOverflow")
  val sc: SparkContext = new SparkContext(conf)
}

class StackOverflowSuite {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    override def langSpread = 50000

    override def kmeansKernels = 45

    override def kmeansEta: Double = 20.0D

    override def kmeansMaxIterations = 120
  }

  @Test def `testObject can be instantiated`: Unit = {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }


  @Test def `scored RDD should have 2121822 entries`: Unit = {
    val lines = StackOverflowSuite.sc.textFile("../stackoverflow.csv")
    val raw = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val scored = scoredPostings(grouped).cache()
    val vectors = vectorPostings(scored) //  RDD[(stackoverflow.LangIndex, stackoverflow.HighScore)]
    assert(vectors.count() == 2121822, "Incorrect number of vectors: " + vectors.count())
    val expected = Set(
      (350000, 67),
      (100000, 89),
      (300000, 3),
      (50000, 30),
      (200000, 20)
    )
    val current = vectors.filter(item => expected.contains(item)).distinct().collect().toSet
    import org.scalatest.matchers.should.Matchers._
    expected shouldEqual current
  }

  @Test def `kmeans first iteration`: Unit = {

    val lines = StackOverflowSuite.sc.textFile("../stackoverflow.csv")
    val raw = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val scored = scoredPostings(grouped).cache()
    val vectors = vectorPostings(scored) //  RDD[(stackoverflow.LangIndex, stackoverflow.HighScore)]
    val means = kmeans(sampleVectors(vectors), vectors, debug = true)

  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(100 * 1000)
}
