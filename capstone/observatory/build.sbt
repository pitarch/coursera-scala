course := "capstone"
assignment := "observatory"

scalaVersion := "2.12.8"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xlint",
)

//resolvers += Resolver.sonatypeRepo("releases")
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"


addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")


libraryDependencies ++= Seq(
  "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.8", // for visualization
  // You don’t *have to* use Spark, but in case you want to, we have added the dependency
  "org.apache.spark" %% "spark-sql" % "2.4.3",
  "org.scalactic" %% "scalactic" % "3.2.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",

// You don’t *have to* use akka-stream, but in case you want to, we have added the dependency
//  "com.typesafe.akka" %% "akka-stream" % "2.6.0",
//  "com.typesafe.akka" %% "akka-stream-testkit" % "2.6.0" % Test,
  // You don’t *have to* use Monix, but in case you want to, we have added the dependency
//  "io.monix" %% "monix" % "2.3.3",
  // You don’t *have to* use fs2, but in case you want to, we have added the dependency
  "co.fs2" %% "fs2-io" % "2.4.0",
  "com.novocode" % "junit-interface" % "0.11" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
  "org.scalatest" %% "scalatest" % "3.2.1" % Test,
 "org.scalatestplus" %% "scalacheck-1-14" % "3.2.1.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.5" % Test,
  "com.storm-enroute" %% "scalameter" % "0.19",

//  "com.holdenkarau" %% "spark-testing-base" % "2.4.3_0.14.0" % Test
)

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")

parallelExecution in Test := false // So that tests are executed for each milestone, one after the other
