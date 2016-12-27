name := "JS Tools"

version := "0.7.1-SNAPSHOT"

organization := "com.schmueckers"

scalaVersion := "2.11.8"
scalaVersion in ThisBuild := "2.11.8"

scalacOptions += "-target:jvm-1.7"

EclipseKeys.eclipseOutput := Some(".target")
EclipseKeys.withSource := true

libraryDependencies += "com.opencsv" % "opencsv" % "3.8"

// add scala-xml dependency when needed (for Scala 2.11 and newer) in a robust way
// this mechanism supports cross-version publishing
// taken from: http://github.com/scala/scala-module-dependency-sample
libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.11+ is used, add dependency on scala-xml module
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
        "org.scala-lang.modules" %% "scala-swing" % "1.0.1")
    case _ =>
      // or just libraryDependencies.value if you don't depend on scala-swing
      libraryDependencies.value //:+ "org.scala-lang" % "scala-swing" % scalaVersion.value
  }
}

//libraryDependencies += "com.sksamuel.elastic4s" % "elastic4s-core_2.11" % "2.1.2" withSources()

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0"

