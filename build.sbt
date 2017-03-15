name := "fpinscala"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.4",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "50", "-workers", "1", "-verbosity", "1")

dependencyOverrides += "org.scala-lang" % "scala-compiler" % scalaVersion.value

//scalacOptions in ThisBuild += "-Xlog-implicits"
