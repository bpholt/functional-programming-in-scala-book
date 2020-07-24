name := "functional-programming-in-scala"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= {
  Seq(
    "org.typelevel" %% "cats-core" % "2.2.0-RC2",
    "org.scalatest" %% "scalatest" % "3.1.0" % Test,
  )
}

scalacOptions -= "-Xfatal-warnings"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
