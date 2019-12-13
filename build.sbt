name := "functional-programming-in-scala"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= {
  Seq(
    "org.typelevel" %% "cats-core" % "2.0.0",
    "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  )
}

scalacOptions -= "-Xfatal-warnings"
