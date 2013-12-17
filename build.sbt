version := "1.0.0-SNAPSHOT"

scalaVersion := "2.10.2"

name := "scala-matrix"

organization := "org.jrosalia"

libraryDependencies ++= Seq(
  //  "org.scalatest" %% "scalatest" % "2.0.M5b",
    "org.scalaz" %% "scalaz-core" % "7.0.3"
//    "commons-io" % "commons-io" % "2.4",
   // "play" %% "play-json" % "2.2-SNAPSHOT"
)

//libraryDependencies <+= (scalaVersion) { "org.scala-lang" % "scala-swing" % _ }

//resolvers += "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/"

scalacOptions ++= Seq("-feature","-deprecation","-language:postfixOps")
