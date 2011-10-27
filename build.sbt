scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "6.0.3",
  "org.apache.commons" % "commons-math" % "2.2",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.2.0",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.2.0",
  "org.specs2" %% "specs2" % "1.6.1",
  "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test"
)

resolvers ++= Seq(
  "snapshots" at "http://scala-tools.org/repo-snapshots",
  "releases"  at "http://scala-tools.org/repo-releases"
)
