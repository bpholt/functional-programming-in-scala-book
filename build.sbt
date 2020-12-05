name := "functional-programming-in-scala"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= {
  val scalaTestV = "3.2.3"
  val refinedV = "0.9.19"
  Seq(
    "org.typelevel" %% "cats-core" % "2.2.0",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.scalatest" %% "scalatest-flatspec" % scalaTestV % Test,
    "org.scalatest" %% "scalatest-shouldmatchers" % scalaTestV % Test,
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.3.0" % Test,
    "eu.timepit" %% "refined" % refinedV % Test,
    "eu.timepit" %% "refined-scalacheck" % refinedV % Test,
    "eu.timepit" %% "refined-cats" % refinedV % Test,
  )
}

scalacOptions -= "-Xfatal-warnings"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

lazy val SCALA_2_13 = "2.13.3"

inThisBuild(List(
  scalaVersion := SCALA_2_13,
  crossScalaVersions := Seq(SCALA_2_13),
  organization := "com.dwolla",
  homepage := Option(url("https://github.com/bpholt/functional-programming-in-scala-book")),
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
