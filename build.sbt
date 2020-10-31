name := "functional-programming-in-scala"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= {
  Seq(
    "org.typelevel" %% "cats-core" % "2.2.0-RC2",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.scalatest" %% "scalatest" % "3.1.0" % Test,
  )
}

scalacOptions -= "-Xfatal-warnings"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

lazy val SCALA_2_13 = "2.13.3"

inThisBuild(List(
  scalaVersion := SCALA_2_13,
  crossScalaVersions := Seq(SCALA_2_13),
  organization := "com.dwolla",
  homepage := Option(url("https://github.com/Dwolla/fs2-pgp")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "bpholt",
      "Brian Holt",
      "bholt@planetholt.com",
      url("https://planetholt.com")
    )
  ),
  githubWorkflowTargetTags ++= Seq("v*"),
  githubWorkflowPublishTargetBranches :=
    Seq(RefPredicate.StartsWith(Ref.Tag("v"))),
))
